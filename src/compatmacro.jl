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

# https://github.com/JuliaLang/julia/pull/50105
macro compat(public::Symbol, symbols_expr::Union{Expr, Symbol})
    public == :public || throw(ArgumentError("Invalid Syntax: `@compat $public $symbols_expr`"))
    symbols = _get_symbols(symbols_expr)
    if VERSION >= v"1.11.0-DEV.469"
        esc(Expr(:public, symbols...))
    end
end

"""
    _valid_macro(expr)

Check if `expr` is a valid macro call with no arguments.
"""
_valid_macro(expr) = Meta.isexpr(expr, :macrocall) && length(expr.args) == 2 &&
    expr.args[1] isa Symbol && string(expr.args[1])[1] == '@' &&
    expr.args[2] isa LineNumberNode

_get_symbols(symbol::Symbol) = [symbol]
function _get_symbols(expr::Expr)
    _valid_macro(expr) && return [expr.args[1]]
    expr.head == :tuple || throw(ArgumentError("cannot mark `$expr` as public. Try `@compat public foo, bar`."))
    symbols = Vector{Symbol}(undef, length(expr.args))
    for (i, arg) in enumerate(expr.args)
        if arg isa Symbol
            symbols[i] = arg
        elseif _valid_macro(arg)
            symbols[i] = arg.args[1]
        else
            throw(ArgumentError("cannot mark `$arg` as public. Try `@compat public foo, bar`."))
        end
    end
    symbols
end
