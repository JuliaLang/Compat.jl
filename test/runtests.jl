using Compat
using Compat.Test
using Compat.LinearAlgebra
using Compat.SparseArrays

@test isempty(detect_ambiguities(Base, Core, Compat))

const struct_sym = VERSION < v"0.7.0-DEV.1263" ? :type : :struct

# julia#29679
@test !isnothing(1)
@test isnothing(nothing)

# https://github.com/JuliaLang/julia/pull/29749
if VERSION >= v"0.7"
    @testset "row/column/slice iterators" begin
        # Simple ones
        M = [1 2 3; 4 5 6; 7 8 9]
        @test collect(eachrow(M)) == collect(eachslice(M, dims = 1)) == [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        @test collect(eachcol(M)) == collect(eachslice(M, dims = 2)) == [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
        @test_throws DimensionMismatch eachslice(M, dims = 4)

        # Higher-dimensional case
        M = reshape([(1:16)...], 2, 2, 2, 2)
        @test_throws MethodError collect(eachrow(M))
        @test_throws MethodError collect(eachcol(M))
        @test collect(eachslice(M, dims = 1))[1][:, :, 1] == [1 5; 3 7]
    end
end

# Support for positional `stop`
@test Compat.range(0, 5, length = 6) == 0.0:1.0:5.0
@test Compat.range(0, 10, step = 2) == 0:2:10

mutable struct TLayout
    x::Int8
    y::Int16
    z::Int32
end
tlayout = TLayout(5,7,11)
@test hasfield(TLayout, :y)
@test !hasfield(TLayout, :a)
if VERSION >= v"0.7-"
    @test hasproperty(tlayout, :x)
    @test !hasproperty(tlayout, :p)
end

@static if VERSION >= v"0.7.0"
    @test merge((a=1,b=1)) == (a=1,b=1)
    @test merge((a=1,), (b=2,), (c=3,)) == (a=1,b=2,c=3)
end

@static if VERSION >= v"0.7.0"
    @testset "only" begin
        @test only([3]) === 3
        @test_throws ArgumentError only([])
        @test_throws ArgumentError only([3, 2])

        @test @inferred(only((3,))) === 3
        @test_throws ArgumentError only(())
        @test_throws ArgumentError only((3, 2))

        @test only(Dict(1=>3)) === (1=>3)
        @test_throws ArgumentError only(Dict{Int,Int}())
        @test_throws ArgumentError only(Dict(1=>3, 2=>2))

        @test only(Set([3])) === 3
        @test_throws ArgumentError only(Set(Int[]))
        @test_throws ArgumentError only(Set([3,2]))

        @test @inferred(only((;a=1))) === 1
        @test_throws ArgumentError only(NamedTuple())
        @test_throws ArgumentError only((a=3, b=2.0))

        @test @inferred(only(1)) === 1
        @test @inferred(only('a')) === 'a'
        if  VERSION >= v"1.0"
            @test @inferred(only(Ref([1, 2]))) == [1, 2] # Fails on v0.7, depwarn "`Ref(x::AbstractArray)` is deprecated, use `Ref(x, 1)` instead."
        end
        @test_throws ArgumentError only(Pair(10, 20))

        @test only(1 for ii in 1:1) === 1
        @test only(1 for ii in 1:10 if ii < 2) === 1
        @test_throws ArgumentError only(1 for ii in 1:10)
        @test_throws ArgumentError only(1 for ii in 1:10 if ii > 2)
        @test_throws ArgumentError only(1 for ii in 1:10 if ii > 200)
    end
end

# https://github.com/JuliaLang/julia/pull/32628
if VERSION >= v"0.7"
    @testset "mod with ranges" begin
        for n in -10:10
            @test mod(n, 0:4) == mod(n, 5)
            @test mod(n, 1:5) == mod1(n, 5)
            @test mod(n, 2:6) == 2 + mod(n-2, 5)
            @test mod(n, Base.OneTo(5)) == mod1(n, 5)
        end
        @test mod(Int32(3), 1:5) == 3
        @test mod(big(typemax(Int))+99, 0:4) == mod(big(typemax(Int))+99, 5)
        @test_throws MethodError mod(3.141, 1:5)
        @test_throws MethodError mod(3, UnitRange(1.0,5.0))
        @test_throws MethodError mod(3, 1:2:7)
        @test_throws DivideError mod(3, 1:0)
    end
end

include("old.jl")

nothing
