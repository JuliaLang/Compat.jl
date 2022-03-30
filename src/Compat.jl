module Compat

import Dates
using Dates: Period, CompoundPeriod

import LinearAlgebra

include("compatmacro.jl")

# NOTE these `@inline` and `@noinline` definitions overwrite the definitions implicitly
# imported from Base and so should happen before any usages of them within this module

# https://github.com/JuliaLang/julia/pull/41312: `@inline`/`@noinline` annotations within a function body
@static if !hasmethod(getfield(Base, Symbol("@inline")), (LineNumberNode,Module))
    macro inline()   Expr(:meta, :inline)   end
    macro noinline() Expr(:meta, :noinline) end
end

# https://github.com/JuliaLang/julia/pull/41328: callsite annotations of inlining
@static if !isdefined(Base, :annotate_meta_def_or_block)
    macro inline(ex)   annotate_meta_def_or_nothing(ex, :inline)   end
    macro noinline(ex) annotate_meta_def_or_nothing(ex, :noinline) end
    function annotate_meta_def_or_nothing(@nospecialize(ex), meta::Symbol)
        inner = unwrap_macrocalls(ex)
        if is_function_def(inner)
            # annotation on a definition
            return esc(Base.pushmeta!(ex, meta))
        else
            # do nothing
            return esc(ex)
        end
    end
    unwrap_macrocalls(@nospecialize(x)) = x
    function unwrap_macrocalls(ex::Expr)
        inner = ex
        while inner.head === :macrocall
            inner = inner.args[end]::Expr
        end
        return inner
    end
    is_function_def(@nospecialize(ex)) =
        return Meta.isexpr(ex, :function) || is_short_function_def(ex) || Meta.isexpr(ex, :->)
    function is_short_function_def(@nospecialize(ex))
        Meta.isexpr(ex, :(=)) || return false
        while length(ex.args) >= 1 && isa(ex.args[1], Expr)
            (ex.args[1].head === :call) && return true
            (ex.args[1].head === :where || ex.args[1].head === :(::)) || return false
            ex = ex.args[1]
        end
        return false
    end
end

if VERSION < v"1.7.0-DEV.119"
    # Part of:
    # https://github.com/JuliaLang/julia/pull/35316
    # https://github.com/JuliaLang/julia/pull/41076
    isunordered(x) = false
    isunordered(x::AbstractFloat) = isnan(x)
    isunordered(x::Missing) = true

    isgreater(x, y) = isunordered(x) || isunordered(y) ? isless(x, y) : isless(y, x)

    Base.findmax(f, domain) = mapfoldl( ((k, v),) -> (f(v), k), _rf_findmax, pairs(domain) )
    _rf_findmax((fm, im), (fx, ix)) = isless(fm, fx) ? (fx, ix) : (fm, im)

    Base.findmin(f, domain) = mapfoldl( ((k, v),) -> (f(v), k), _rf_findmin, pairs(domain) )
    _rf_findmin((fm, im), (fx, ix)) = isgreater(fm, fx) ? (fx, ix) : (fm, im)

    Base.argmax(f, domain) = mapfoldl(x -> (f(x), x), _rf_findmax, domain)[2]
    Base.argmin(f, domain) = mapfoldl(x -> (f(x), x), _rf_findmin, domain)[2]
end

# https://github.com/JuliaLang/julia/pull/40729
if VERSION < v"1.7.0-DEV.1088"
    macro something(args...)
        expr = :(nothing)
        for arg in reverse(args)
            expr = :((val = $arg) !== nothing ? val : $expr)
        end
        return esc(:(something(let val; $expr; end)))
    end

    macro coalesce(args...)
        expr = :(missing)
        for arg in reverse(args)
            expr = :((val = $arg) !== missing ? val : $expr)
        end
        return esc(:(let val; $expr; end))
    end

    export @something, @coalesce
end

# https://github.com/JuliaLang/julia/pull/41007
if VERSION < v"1.7.0-DEV.1220"
    Base.get(f::Base.Callable, A::AbstractArray, i::Integer) = checkbounds(Bool, A, i) ? A[i] : f()
    Base.get(f::Base.Callable, A::AbstractArray, I::Tuple{}) = checkbounds(Bool, A) ? A[] : f()
    Base.get(f::Base.Callable, A::AbstractArray, I::Dims) = checkbounds(Bool, A, I...) ? A[I...] : f()

    Base.get(t::Tuple, i::Integer, default) = i in 1:length(t) ? getindex(t, i) : default
    Base.get(f::Base.Callable, t::Tuple, i::Integer) = i in 1:length(t) ? getindex(t, i) : f()
end

# https://github.com/JuliaLang/julia/pull/41032
if VERSION < v"1.7.0-DEV.1230"
    Base.get(x::Number, i::Integer, default) = isone(i) ? x : default
    Base.get(x::Number, ind::Tuple, default) = all(isone, ind) ? x : default
    Base.get(f::Base.Callable, x::Number, i::Integer) = isone(i) ? x : f()
    Base.get(f::Base.Callable, x::Number, ind::Tuple) = all(isone, ind) ? x : f()
end

# https://github.com/JuliaLang/julia/pull/29901
if VERSION < v"1.7.0-DEV.1106"
    struct ExceptionStack <: AbstractArray{Any,1}
        stack
    end

    function current_exceptions(task=current_task(); backtrace=true)
        old_stack = Base.catch_stack(task, include_bt=backtrace)
        # If include_bt=true, Base.catch_stack yields a Vector of two-tuples,
        # where the first element of each tuple is an exception and the second
        # element is the corresponding backtrace. If instead include_bt=false,
        # Base.catch_stack yields a Vector of exceptions.
        #
        # Independent of its backtrace keyword argument, Base.current_exceptions
        # yields an ExceptionStack that wraps a Vector of two-element
        # NamedTuples, where the first element of each named tuple is an exception
        # and the second element is either a correpsonding backtrace or `nothing`.
        #
        # The following constructs the ExceptionStack-wrapped Vector appropriately.
        new_stack = backtrace ?
            Any[(exception=exc_and_bt[1], backtrace=exc_and_bt[2]) for exc_and_bt in old_stack] :
            Any[(exception=exc_only,      backtrace=nothing) for exc_only in old_stack]
        return ExceptionStack(new_stack)
    end

    Base.size(s::ExceptionStack) = size(s.stack)
    Base.getindex(s::ExceptionStack, i::Int) = s.stack[i]

    function show_exception_stack(io::IO, stack)
        # Display exception stack with the top of the stack first.  This ordering
        # means that the user doesn't have to scroll up in the REPL to discover the
        # root cause.
        nexc = length(stack)
        for i = nexc:-1:1
            if nexc != i
                printstyled(io, "\ncaused by: ", color=Base.error_color())
            end
            exc, bt = stack[i]
            showerror(io, exc, bt, backtrace = bt!==nothing)
            i == 1 || println(io)
        end
    end

    function Base.display_error(io::IO, stack::ExceptionStack)
        printstyled(io, "ERROR: "; bold=true, color=Base.error_color())
        # Julia >=1.2 provides Base.scrub_repl_backtrace; we use it
        # where possible and otherwise leave backtraces unscrubbed.
        backtrace_scrubber = VERSION >= v"1.2" ? Base.scrub_repl_backtrace : identity
        bt = Any[ (x[1], backtrace_scrubber(x[2])) for x in stack ]
        show_exception_stack(IOContext(io, :limit => true), bt)
        println(io)
    end

    function Base.show(io::IO, ::MIME"text/plain", stack::ExceptionStack)
        nexc = length(stack)
        printstyled(io, nexc, "-element ExceptionStack", nexc == 0 ? "" : ":\n")
        show_exception_stack(io, stack)
    end
    Base.show(io::IO, stack::ExceptionStack) = show(io, MIME("text/plain"), stack)

    export current_exceptions
end

# https://github.com/JuliaLang/julia/pull/39794
if VERSION < v"1.7.0-DEV.793"
    export Returns

    struct Returns{V} <: Function
        value::V
        Returns{V}(value) where {V} = new{V}(value)
        Returns(value) = new{Core.Typeof(value)}(value)
    end

    (obj::Returns)(args...; kw...) = obj.value
    function Base.show(io::IO, obj::Returns)
        show(io, typeof(obj))
        print(io, "(")
        show(io, obj.value)
        print(io, ")")
    end
end

# https://github.com/JuliaLang/julia/pull/39037
if VERSION < v"1.7.0-DEV.204"
    # Borrowed from julia base
    export ismutabletype
    function ismutabletype(@nospecialize(t::Type))
        t = Base.unwrap_unionall(t)
        # TODO: what to do for `Union`?
        return isa(t, DataType) && t.mutable
    end
end

# https://github.com/JuliaLang/julia/pull/42125
if !isdefined(Base, Symbol("@constprop"))
    if isdefined(Base, Symbol("@aggressive_constprop"))
        macro constprop(setting, ex)
            if isa(setting, QuoteNode)
                setting = setting.value
            end
            setting === :aggressive && return esc(:(Base.@aggressive_constprop $ex))
            setting === :none && return esc(ex)
            throw(ArgumentError("@constprop $setting not supported"))
        end
    else
        macro constprop(setting, ex)
            if isa(setting, QuoteNode)
                setting = setting.value
            end
            setting === :aggressive || setting === :none || throw(ArgumentError("@constprop $setting not supported"))
            return esc(ex)
        end
    end
else
    using Base: @constprop
end

# https://github.com/JuliaLang/julia/pull/40803
if VERSION < v"1.8.0-DEV.300"
    function Base.convert(::Type{T}, x::CompoundPeriod) where T<:Period
        return isconcretetype(T) ? sum(T, x.periods) : throw(MethodError(convert, (T, x)))
    end
end

# https://github.com/JuliaLang/julia/pull/39245
if VERSION < v"1.8.0-DEV.487"  
    export eachsplit
    
    """
        eachsplit(str::AbstractString, dlm; limit::Integer=0)
        eachsplit(str::AbstractString; limit::Integer=0)

    Split `str` on occurrences of the delimiter(s) `dlm` and return an iterator over the
    substrings.  `dlm` can be any of the formats allowed by [`findnext`](@ref)'s first argument
    (i.e. as a string, regular expression or a function), or as a single character or collection
    of characters.
    
    If `dlm` is omitted, it defaults to [`isspace`](@ref).
    
    The iterator will return a maximum of `limit` results if the keyword argument is supplied.
    The default of `limit=0` implies no maximum.
    
    See also [`split`](@ref).
    
    # Examples
    ```julia
    julia> a = "Ma.rch"
    "Ma.rch"
    julia> collect(eachsplit(a, "."))
    2-element Vector{SubString}:
    "Ma"
    "rch"
    ```
    """
    function eachsplit end
    
    struct SplitIterator{S<:AbstractString,F}
        str::S
        splitter::F
        limit::Int
        keepempty::Bool
    end

    Base.eltype(::Type{<:SplitIterator}) = SubString
    Base.IteratorSize(::Type{<:SplitIterator}) = Base.SizeUnknown()
    
    function Base.iterate(iter::SplitIterator, (i, k, n)=(firstindex(iter.str), firstindex(iter.str), 0))
        i - 1 > ncodeunits(iter.str)::Int && return nothing
        r = findnext(iter.splitter, iter.str, k)::Union{Nothing,Int,UnitRange{Int}}
        while r !== nothing && n != iter.limit - 1 && first(r) <= ncodeunits(iter.str)
            r = r::Union{Int,UnitRange{Int}} #commit dcc2182db228935fe97d03a44ae3b6889e40c542
            #follow #39245, improve inferrability of iterate(::SplitIterator)            
            #Somehow type constraints from the complex `while` condition don't
            #propagate to the `while` body.
            j, k = first(r), nextind(iter.str, last(r))::Int
            k_ = k <= j ? nextind(iter.str, j) : k
            if i < k
                substr = @inbounds SubString(iter.str, i, prevind(iter.str, j)::Int)
                (iter.keepempty || i < j) && return (substr, (k, k_, n + 1))
                i = k
            end
            k = k_
            r = findnext(iter.splitter, iter.str, k)::Union{Nothing,Int,UnitRange{Int}}
        end
        iter.keepempty || i <= ncodeunits(iter.str) || return nothing
        @inbounds SubString(iter.str, i), (ncodeunits(iter.str) + 2, k, n + 1)
    end

    eachsplit(str::T, splitter; limit::Integer=0, keepempty::Bool=true) where {T<:AbstractString} =
        SplitIterator(str, splitter, limit, keepempty)

    eachsplit(str::T, splitter::Union{Tuple{Vararg{AbstractChar}},AbstractVector{<:AbstractChar},Set{<:AbstractChar}};
            limit::Integer=0, keepempty=true) where {T<:AbstractString} =
        eachsplit(str, in(splitter); limit=limit, keepempty=keepempty)

    eachsplit(str::T, splitter::AbstractChar; limit::Integer=0, keepempty=true) where {T<:AbstractString} =
        eachsplit(str, isequal(splitter); limit=limit, keepempty=keepempty)

    eachsplit(str::AbstractString; limit::Integer=0, keepempty=false) =
        eachsplit(str, isspace; limit=limit, keepempty=keepempty)
end

# https://github.com/JuliaLang/julia/pull/43354
if VERSION < v"1.8.0-DEV.1494" # 98e60ffb11ee431e462b092b48a31a1204bd263d
    export allequal
    allequal(itr) = isempty(itr) ? true : all(isequal(first(itr)), itr)
    allequal(c::Union{AbstractSet,AbstractDict}) = length(c) <= 1
    allequal(r::AbstractRange) = iszero(step(r)) || length(r) <= 1
end

include("deprecated.jl")

end # module Compat
