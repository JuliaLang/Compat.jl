# The @compat macro is used to implement compatibility rules that require
# syntax rewriting rather than simply new function/constant/module definitions.

export @compat

function _compat(ex::Expr)
    if ex.head === :quote && isa(ex.args[1], Symbol)
        # Passthrough
        return ex
    end

    # https://github.com/JuliaLang/julia/pull/39285
    @static if VERSION < v"1.7.0-DEV.364"
        if Meta.isexpr(ex, :(=)) && Meta.isexpr(ex.args[1], :tuple) &&
            Meta.isexpr(ex.args[1].args[1], :parameters)
            
            ex = _destructure_named_tuple(ex)
        end
    end
    
    return Expr(ex.head, map(_compat, ex.args)...)
end

_compat(ex) = ex

macro compat(ex)
    esc(_compat(ex))
end

function _destructure_named_tuple(ex::Expr)
    ex.args[1].args[1] isa Expr && ex.args[1].args[1].head === :parameters
    values = ex.args[2]
    parameters = ex.args[1].args[1].args
    ex = Expr(:block)
    for p in parameters
        asgn = Expr(:(=), p, Expr(:., values, QuoteNode(p)))
        push!(ex.args, asgn)
    end
    push!(ex.args, values)
    return ex
end
