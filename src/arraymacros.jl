# Julia 0.6 macros to aid in vectorization: @view, @views, @__dot__ (@.),
# backported from Julia 0.6.

# prior to julia#20247, the replace_ref_end! macro had hygiene bugs

if !isdefined(Base, Symbol("@view"))
    macro view(ex)
        if Meta.isexpr(ex, :ref)
            ex = replace_ref_end!(ex)
            if Meta.isexpr(ex, :ref)
                ex = Expr(:call, view, ex.args...)
            else # ex replaced by let ...; foo[...]; end
                assert(Meta.isexpr(ex, :let) && Meta.isexpr(ex.args[1], :ref))
                ex.args[1] = Expr(:call, view, ex.args[1].args...)
            end
            Expr(:&&, true, esc(ex))
        else
            throw(ArgumentError("Invalid use of @view macro: argument must be a reference expression A[...]."))
        end
    end
    export @view
end

if !isdefined(Base, Symbol("@views"))
    maybeview(A, args...) = getindex(A, args...)
    maybeview(A::AbstractArray, args...) = view(A, args...)
    maybeview(A::AbstractArray, args::Number...) = getindex(A, args...)
    maybeview(A) = getindex(A)
    maybeview(A::AbstractArray) = getindex(A)

    _views(x) = x
    function _views(ex::Expr)
        if ex.head in (:(=), :(.=))
            # don't use view for ref on the lhs of an assignment,
            # but still use views for the args of the ref:
            lhs = ex.args[1]
            Expr(ex.head, Meta.isexpr(lhs, :ref) ?
                          Expr(:ref, map(_views, lhs.args)...) : _views(lhs),
                 _views(ex.args[2]))
        elseif ex.head == :ref
            Expr(:call, maybeview, map(_views, ex.args)...)
        else
            h = string(ex.head)
            # don't use view on the lhs of an op-assignment a[i...] += ...
            if last(h) == '=' && Meta.isexpr(ex.args[1], :ref)
                lhs = ex.args[1]

                # temp vars to avoid recomputing a and i,
                # which will be assigned in a let block:
                a = gensym(:a)
                i = [gensym(:i) for k = 1:length(lhs.args)-1]

                # for splatted indices like a[i, j...], we need to
                # splat the corresponding temp var.
                I = similar(i, Any)
                for k = 1:length(i)
                    if Meta.isexpr(lhs.args[k+1], :...)
                        I[k] = Expr(:..., i[k])
                        lhs.args[k+1] = lhs.args[k+1].args[1] # unsplat
                    else
                        I[k] = i[k]
                    end
                end

                Expr(:let,
                     Expr(first(h) == '.' ? :(.=) : :(=), :($a[$(I...)]),
                          Expr(:call, Symbol(h[1:end-1]),
                               :($maybeview($a, $(I...))),
                               map(_views, ex.args[2:end])...)),
                     :($a = $(_views(lhs.args[1]))),
                     [:($(i[k]) = $(_views(lhs.args[k+1]))) for k=1:length(i)]...)
            else
                Expr(ex.head, map(_views, ex.args)...)
            end
        end
    end

    macro views(x)
        esc(_views(replace_ref_end!(x)))
    end
    export @views
end

# we can't define @. because that doesn't parse in Julia < 0.6, but
# we can define @__dot__, which is what @. is sugar for:
if !isdefined(Base, Symbol("@__dot__"))
    dottable(x) = false # avoid dotting spliced objects (e.g. view calls inserted by @view)
    dottable(x::Symbol) = !Base.isoperator(x) || first(string(x)) != '.' || x == :.. # don't add dots to dot operators
    dottable(x::Expr) = x.head != :$
    undot(x) = x
    function undot(x::Expr)
        if x.head == :.=
            Expr(:(=), x.args...)
        elseif x.head == :block # occurs in for x=..., y=...
            Expr(:block, map(undot, x.args)...)
        else
            x
        end
    end
    __dot__(x) = x
    function __dot__(x::Expr)
        dotargs = map(__dot__, x.args)
        if x.head == :call && dottable(x.args[1])
            Expr(:., dotargs[1], Expr(:tuple, dotargs[2:end]...))
        elseif x.head == :$
            x.args[1]
        elseif x.head == :let # don't add dots to "let x=... assignments
            Expr(:let, dotargs[1], map(undot, dotargs[2:end])...)
        elseif x.head == :for # don't add dots to for x=... assignments
            Expr(:for, undot(dotargs[1]), dotargs[2])
        elseif (x.head == :(=) || x.head == :function || x.head == :macro) &&
               Meta.isexpr(x.args[1], :call) # function or macro definition
            Expr(x.head, x.args[1], dotargs[2])
        else
            head = string(x.head)
            if last(head) == '=' && first(head) != '.'
                Expr(Symbol('.',head), dotargs...)
            else
                Expr(x.head, dotargs...)
            end
        end
    end
    macro __dot__(x)
        esc(__dot__(x))
    end
    macro dotcompat(x)
        esc(_compat(__dot__(x)))
    end
    export @__dot__, @dotcompat
else
    # in 0.6, use the __dot__ function from Base.Broadcast
    macro dotcompat(x)
        esc(_compat(Base.Broadcast.__dot__(x)))
    end
    export @dotcompat
end
