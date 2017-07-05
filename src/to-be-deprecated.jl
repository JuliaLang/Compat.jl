# Uncomment the depwarns when we drop 0.4 support
# See also ngenerate.jl.
macro Dict(pairs...)
    esc(Expr(:block, :(Base.depwarn("@Dict is deprecated, use Dict instead", Symbol("@Dict"))),
                       Expr(:call, :Dict, pairs...)))
end
macro AnyDict(pairs...)
    esc(Expr(:block, :(Base.depwarn("@AnyDict is deprecated, use Dict{Any,Any} instead",
                                    Symbol("@AnyDict"))),
             Expr(:call, :(Base.AnyDict), pairs...)))
end

include("ngenerate.jl")
using .CompatCartesian
export @ngenerate, @nsplat

export @functorize
macro functorize(f)
    code = f === :scalarmax          ? :(Base.scalarmax) :
           f === :scalarmin          ? :(Base.scalarmin) :
           f === :centralizedabs2fun ? :(primarytype(typeof(Base.centralizedabs2fun(0)))) :
           f

    return quote
        Base.depwarn("@functorize is deprecated as functor objects are no longer supported in julia", Symbol("@functorize"))
        $code
    end
end

# More things that could be removed in Compat.jl
# - new_style_call_overload
# - import Base.Filesystem
# - UTF8String
# - ASCIIString
# - KERNEL
# - import Base.LinAlg.BLAS.@blasfunc
