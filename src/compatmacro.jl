# The @compat macro is used to implement compatibility rules that require
# syntax rewriting rather than simply new function/constant/module definitions.

using Base.Meta
export @compat

function _compat(ex::Expr)
    if ex.head === :call
        f = ex.args[1]
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
