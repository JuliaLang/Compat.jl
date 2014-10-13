using Compat
using Base.Test

v = 1

d = Dict{Int,Int}()
d[1] = 1
@test Compat.@Dict(1 => 1) == d
@test Compat.@Dict(1 => v) == d

ad = Dict{Any,Any}()
ad[1] = 1
@test Compat.@AnyDict(1 => 1) == ad
@test Compat.@AnyDict(1 => v) == ad

td = Compat.@TypedDict( Symbol=>Any, :a=> 1 )
@test typeof( td ) == Dict{Symbol,Any}
@test td[:a] == 1

td = Compat.@TypedDict( Symbol=>Any )
@test typeof( td ) == Dict{Symbol,Any}
@test length( td ) == 0
