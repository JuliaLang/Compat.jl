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
