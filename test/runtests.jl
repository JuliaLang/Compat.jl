using Compat
using Base.Test

v = 1

d = Dict{Int,Int}()
d[1] = 1
@test Compat.@Dict(1 => 1) == d
@test Compat.@Dict(1 => v) == d

@test typeof(@compat(Dict(1 => 1))) == Dict{Int,Int}
@test @compat(Dict(1 => 1)) == d
@test @compat(Dict(1 => v)) == d

ad = Dict{Any,Any}()
ad[1] = 1
@test Compat.@AnyDict(1 => 1) == ad
@test Compat.@AnyDict(1 => v) == ad

@test typeof(@compat(Dict{Any,Any}(1 => 1))) == Dict{Any,Any}
@test @compat(Dict{Any,Any}(1 => 1)) == ad
@test @compat(Dict{Any,Any}(1 => v)) == ad

td = Dict{Int,Float64}()
td[1] = 1.0

@test typeof(@compat(Dict{Int,Float64}(1 => 1))) == Dict{Int,Float64}
@test @compat(Dict{Int,Float64}(1 => 1)) == td
@test @compat(Dict{Int,Float64}(1 => v)) == td

@test @compat(Dict()) == Dict()
@test @compat(Dict{Any,Any}()) == Dict{Any,Any}()
if VERSION >= v"0.3.0-"
	@test @compat(Dict([(1, 1)])) == d
end

d2 = Dict{Symbol,Dict{Symbol,Int}}()
d2[:a] = Dict{Symbol,Int}()
d2[:a][:b] = 1
@test @compat(Dict(:a => Dict(:b => 1))) == d2

d = Dict(zip([1, 2], [3, 4]))
@test d == @compat Dict(1=>3, 2=>4)

@compat function f()
	a = :a
	b = Dict(:b => 1)
	Dict(a => b)
end
@test f() == d2

ns = length(d.slots)
@test length(sizehint!(d, ns + 1).slots) > ns

@test @compat split("a,b,,c", ',', limit=2) == ["a", "b,,c"]
@test @compat split("a,b,,c", ',', limit=2,keep=true) == ["a", "b,,c"]
@test @compat split("a,b,,c", ',', keep=false) == ["a", "b", "c"]
@test @compat split("a,b,,c", ',', keep=true) == ["a", "b", "", "c"]

@test @compat rsplit("a,b,,c", ',', limit=2) == ["a,b,", "c"]
@test @compat rsplit("a,b,,c", ',', limit=2,keep=true) == ["a,b,", "c"]
@test @compat rsplit("a,b,,c", ',', keep=false) == ["a", "b", "c"]
@test @compat rsplit("a,b,,c", ',', keep=true) == ["a", "b", "", "c"]

if VERSION < v"0.4.0-dev+1387"
    @test isdefined(Main, :AbstractString)
end

@test round(Int, 3//4) == 1

@test IPv4("1.2.3.4") == ip"1.2.3.4"
@test IPv6("2001:1:2:3::1") == ip"2001:1:2:3::1"
@test isless(ip"1.2.3.4", ip"1.2.3.5")

@test startswith("abcdef","abc") == true
@test startswith("abcdef","def") == false

@test size(bitrand(3, 4)) == (3, 4)
@test size(bitrand((3, 4))) == (3, 4)
@test size(bitrand(MersenneTwister(), 3, 4)) == (3, 4)
@test size(bitrand(MersenneTwister(), (3, 4))) == (3, 4)
@test rand(Bool) in [false, true]

module CartesianTest
	using Base.Cartesian, Compat
	@ngenerate N NTuple{N,Int} function f(X::NTuple{N,Int}...)
		@ncall N tuple X
	end
end

@test CartesianTest.f(1) == (1,)
@test CartesianTest.f(1,2) == (1,2)
@test CartesianTest.f(1,2,3) == (1,2,3)
@test CartesianTest.f(1,2,3,4) == (1,2,3,4)
@test CartesianTest.f(1,2,3,4,5) == (1,2,3,4,5)

# eltype tests

immutable ETest{S}
    el::S
end

Base.eltype{S}(::Type{ETest{S}}) = S

@test eltype(ETest(1)) == Int
@test eltype(ETest("a")) == ASCIIString
