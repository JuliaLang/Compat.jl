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

# Only the import-renaming usage results in multiple arguments.
# On versions where renaming is supported natively it parses as a
# single expression and thus takes the single-argument @compat
# code-path above
macro compat(ex, exprs...)
    @assert VERSION < v"1.6.0-DEV.1157"
    if length(exprs) == 2 && Meta.isexpr(ex, :import, 1) && Meta.isexpr(ex.args[1], :.) &&
        exprs[1] === :as && exprs[2] isa Symbol
        # Matches "import Module(.Submodule) as a"
        lhs = Symbol[ex.args[1].args...]
        alias = exprs[2]
        return _create_import_expression([lhs => alias])
    elseif Meta.isexpr(ex, [:import, :using], 1) &&
           Meta.isexpr(ex.args[1], :(:)) && length(exprs) % 2 == 0 &&
           all(x -> x === :as, exprs[1:2:end-1]) &&
           all(x -> Meta.isexpr(x, :tuple, 2), exprs[2:2:end-2]) &&
           exprs[end] isa Symbol
        # Matches "(import|using) Module(.Submodule): a as b(, c as d)
        syms = Symbol[ ex.args[1].args[2].args[1] ]
        foreach(x -> x isa Symbol ? push!(syms, x) : append!(syms, x.args), exprs[2:2:end])

        path_aliases = Pair{Vector{Symbol},Symbol}[]
        init = ex.args[1].args[1].args
        for i in 1:2:length(syms)
            push!(path_aliases, Symbol[init; syms[i]] => syms[i+1])
        end
        return _create_import_expression(path_aliases)
    else
        throw(ArgumentError("invalid use of @compat"))
    end
end

function _create_import_expression(path_aliases::Vector{Pair{Vector{Symbol}, Symbol}})
    # Create an gensymd baremodule to hide names in
    s = gensym()
    # Create all import/const exprs
    import_exprs = Expr[]
    const_exprs = Expr[]
    for (path, alias) in path_aliases
        import_expr = Expr(:import, Expr(:., path...))
        push!(import_exprs, import_expr)
        rhs_expr = Expr(:escape, Expr(:., s, QuoteNode(last(path))))
        const_expr = Expr(:const, Expr(:global, Expr(:(=), alias, rhs_expr)))
        push!(const_exprs, const_expr)
    end
    module_expr = Expr(:module, false, Expr(:escape, s), Expr(:block, import_exprs...))
    return_expr = Expr(:toplevel, module_expr, const_exprs..., nothing)
    return return_expr
end
