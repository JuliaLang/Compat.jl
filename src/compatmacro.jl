# The @compat macro is used to implement compatibility rules that require
# syntax rewriting rather than simply new function/constant/module definitions.

using Base.Meta
export @compat

"""Get just the function part of a function declaration."""
withincurly(ex) = isexpr(ex, :curly) ? ex.args[1] : ex

istopsymbol(ex, mod, sym) = ex in (sym, Expr(:(.), mod, Expr(:quote, sym)))

if !isdefined(Base, :UndefKeywordError)
    struct UndefKeywordError <: Exception
        kw
    end
    Base.showerror(io::IO, e::UndefKeywordError) = print(io, "UndefKeywordError: keyword argument $(e.kw) not assigned")
    export UndefKeywordError
end

"Convert a functions symbol argument to the corresponding required keyword argument."
function symbol2kw(sym::Symbol)
    Expr(:kw, sym, Expr(:call, throw, UndefKeywordError(sym)))
end
symbol2kw(arg) = arg

function _compat(ex::Expr)
    if ex.head === :call
        f = ex.args[1]
        if !isdefined(Base, :UndefKeywordError) && length(ex.args) > 1 && isexpr(ex.args[2], :parameters)
            params = ex.args[2]
            params.args = map(symbol2kw, params.args)
        end
    elseif ex.head === :quote && isa(ex.args[1], Symbol)
        # Passthrough
        return ex
    end
    if VERSION < v"0.7.0-DEV.880"
        if ex.head == :curly && ex.args[1] == :CartesianRange && length(ex.args) >= 2
            a = ex.args[2]
            if a != :CartesianIndex && !(isa(a, Expr) && a.head == :curly && a.args[1] == :CartesianIndex)
                return Expr(:curly, :CartesianRange, Expr(:curly, :CartesianIndex, ex.args[2]))
            end
        end
    end
    if VERSION < v"0.7.0-DEV.2562"
        if ex.head == :call && ex.args[1] == :finalizer
            ex.args[2], ex.args[3] = ex.args[3], ex.args[2]
        end
    end
    return Expr(ex.head, map(_compat, ex.args)...)
end

_compat(ex) = ex

macro compat(ex)
    esc(_compat(ex))
end
