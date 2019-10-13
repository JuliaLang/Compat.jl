# The @compat macro is used to implement compatibility rules that require
# syntax rewriting rather than simply new function/constant/module definitions.

export @compat

function _compat(ex::Expr)
    if ex.head === :quote && isa(ex.args[1], Symbol)
        # Passthrough
        return ex
    end
    return Expr(ex.head, map(_compat, ex.args)...)
end

_compat(ex) = ex

macro compat(ex)
    esc(_compat(ex))
end
