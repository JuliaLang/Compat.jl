# The @compat macro is used to implement compatibility rules that require
# syntax rewriting rather than simply new function/constant/module definitions.

using Base.Meta
export @compat

"""Get just the function part of a function declaration."""
withincurly(ex) = isexpr(ex, :curly) ? ex.args[1] : ex

is_index_style(ex::Expr) = ex == :(Compat.IndexStyle) || ex == :(Base.IndexStyle) ||
    (ex.head == :(.) && (ex.args[1] == :Compat || ex.args[1] == :Base) &&
         ex.args[2] == Expr(:quote, :IndexStyle))

is_index_style(arg) = false

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
    if VERSION < v"0.6.0-dev.2840"
        if ex.head == :(=) && isa(ex.args[1], Expr) && ex.args[1].head == :call
            a = ex.args[1].args[1]
            if is_index_style(a)
                ex.args[1].args[1] = :(Base.linearindexing)
            elseif isa(a, Expr) && a.head == :curly
                if is_index_style(a.args[1])
                    ex.args[1].args[1].args[1] = :(Base.linearindexing)
                end
            end
        end
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

function _get_typebody(ex::Expr)
    args = ex.args
    if ex.head !== :type || length(args) != 3 || args[1] !== true
        throw(ArgumentError("Invalid usage of @compat: $ex"))
    end
    name = args[2]
    if !isexpr(args[3], :block)
        throw(ArgumentError("Invalid type declaration: $ex"))
    end
    body = (args[3]::Expr).args
    filter!(body) do e
        if isa(e, LineNumberNode) || isexpr(e, :line)
            return false
        end
        return true
    end
    return name, body
end

function _compat_primitive(typedecl)
    name, body = _get_typebody(typedecl)
    if length(body) != 1
        throw(ArgumentError("Invalid primitive type declaration: $typedecl"))
    end
    return Expr(:bitstype, body[1], name)
end

function _compat_abstract(typedecl)
    name, body = _get_typebody(typedecl)
    if length(body) != 0
        throw(ArgumentError("Invalid abstract type declaration: $typedecl"))
    end
    return Expr(:abstract, name)
end

macro compat(ex...)
    if VERSION < v"0.6.0-dev.2746" && length(ex) == 2 && ex[1] === :primitive
        return esc(_compat_primitive(ex[2]))
    elseif length(ex) != 1
        throw(ArgumentError("@compat called with wrong number of arguments: $ex"))
    elseif (VERSION < v"0.6.0-dev.2746" && isexpr(ex[1], :abstract) &&
            length(ex[1].args) == 1 && isexpr(ex[1].args[1], :type))
        # This can in principle be handled in nested case but we do not
        # do that to be consistent with primitive types.
        return esc(_compat_abstract(ex[1].args[1]))
    end
    esc(_compat(ex[1]))
end
