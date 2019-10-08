using Compat
using Compat.Test
using Compat.LinearAlgebra
using Compat.SparseArrays

@test isempty(detect_ambiguities(Base, Core, Compat))

const struct_sym = VERSION < v"0.7.0-DEV.1263" ? :type : :struct

let s = "Koala test: 🐨"
    @test transcode(UInt16, s) == UInt16[75,111,97,108,97,32,116,101,115,116,58,32,55357,56360]
    @test transcode(UInt32, s) == UInt32[75,111,97,108,97,32,116,101,115,116,58,32,128040]
    for T in (UInt8,UInt16,UInt32,Cwchar_t)
        @test transcode(Compat.String, transcode(T, s)) == s
        @test transcode(UInt8, transcode(T, s)) == codeunits(s)
        @test transcode(T, s) == transcode(T, codeunits(s)) == transcode(T, transcode(T, s))
    end
end

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

let
    x = view(1:10, 2:4)
    D = Diagonal(x)
    @test D[1,1] == 2
    @test D[3,3] == 4
    A = view(rand(5,5), 1:3, 1:3)
    @test D*A == Diagonal(copy(x)) * copy(A)
    @test A*D == copy(A) * Diagonal(copy(x))
end

# julia#13998
for x in (3.1, -17, 3//4, big(111.1), Inf)
    local x
    @test min(x) == max(x) == x
    @test minmax(x) == (x, x)
end

let
    @compat cr(::CartesianIndices{2}) = 2
    @test cr(CartesianIndices((5, 3))) == 2
    @test_throws MethodError cr(CartesianIndices((5, 3, 2)))
end
if VERSION < v"0.7.0-DEV.880"
    # ensure we don't bork any non-updated expressions
    let
        @compat cr(::CartesianRange{CartesianIndex{2}}) = 2
        @test cr(CartesianRange((5, 3))) == 2
        @test_throws MethodError cr(CartesianRange((5, 3, 2)))
    end
end

# 0.7
let A = [1]
    local x = 0
    @compat finalizer(a->(x+=1), A)
    finalize(A)
    @test x == 1
    A = 0
    GC.gc(); GC.gc()
    @test x == 1
end

@test codeunit("foo") == codeunit(SubString("fooαβγ",1,3)) == UInt8

# 0.7.0-DEV.5278
@test something(nothing, 1) === 1
@test something(Some(2)) === 2
@test something(Some(2), 1) === 2
@test something(nothing, Some(1)) === 1

# julia#24999
let s = "∀α>β:α+"
    @test [length(s,i,j) for i=1:ncodeunits(s)+1, j=0:ncodeunits(s)] ==
        [0 1 1 1 2 2 3 4 4 5 6 6 7; 0 0 0 0 1 1 2 3 3 4 5 5 6; 0 0 0 0 1 1 2 3 3 4 5 5 6; 0 0 0 0 1 1 2 3 3 4 5 5 6; 0 0 0 0 0 0 1 2 2 3 4 4 5; 0 0 0 0 0 0 1 2 2 3 4 4 5; 0 0 0 0 0 0 0 1 1 2 3 3 4; 0 0 0 0 0 0 0 0 0 1 2 2 3; 0 0 0 0 0 0 0 0 0 1 2 2 3; 0 0 0 0 0 0 0 0 0 0 1 1 2; 0 0 0 0 0 0 0 0 0 0 0 0 1; 0 0 0 0 0 0 0 0 0 0 0 0 1; 0 0 0 0 0 0 0 0 0 0 0 0 0]
end
@test_throws BoundsError length("hello", 1, -1)
@test_throws BoundsError length("hellø", 1, -1)
@test_throws BoundsError length("hello", 1, 10)
@test_throws BoundsError length("hellø", 1, 10) == 9
@test_throws BoundsError prevind("hello", 0, 1)
@test_throws BoundsError prevind("hellø", 0, 1)
@test nextind("hello", 0, 10) == 10
# julia#24414
let strs = Any["∀α>β:α+1>β", SubString("123∀α>β:α+1>β123", 4, 18)]
    for s in strs
        @test_throws BoundsError thisind(s, -2)
        @test_throws BoundsError thisind(s, -1)
        @test thisind(s, 0) == 0
        @test thisind(s, 1) == 1
        @test thisind(s, 2) == 1
        @test thisind(s, 3) == 1
        @test thisind(s, 4) == 4
        @test thisind(s, 5) == 4
        @test thisind(s, 6) == 6
        @test thisind(s, 15) == 15
        @test thisind(s, 16) == 15
        @test thisind(s, 17) == 17
        @test_throws BoundsError thisind(s, 18)
        @test_throws BoundsError thisind(s, 19)
    end
end
let strs = Any["", SubString("123", 2, 1)]
    for s in strs
        @test_throws BoundsError thisind(s, -1)
        @test thisind(s, 0) == 0
        @test thisind(s, 1) == 1
        @test_throws BoundsError thisind(s, 2)
    end
end
# prevind and nextind, julia#23805
let s = "∀α>β:α+1>β"
    @test_throws BoundsError prevind(s, 0, 0)
    @test_throws BoundsError prevind(s, 0, 1)
    @test prevind(s, 1, 1) == 0
    @test prevind(s, 1, 0) == 1
    @test prevind(s, 2, 1) == 1
    @test prevind(s, 4, 1) == 1
    @test prevind(s, 5, 1) == 4
    @test prevind(s, 5, 2) == 1
    @test prevind(s, 5, 3) == 0
    @test prevind(s, 15, 1) == 14
    @test prevind(s, 15, 2) == 13
    @test prevind(s, 15, 3) == 12
    @test prevind(s, 15, 4) == 10
    @test prevind(s, 15, 10) == 0
    @test prevind(s, 15, 9) == 1
    @test prevind(s, 16, 1) == 15
    @test prevind(s, 16, 2) == 14
    @test prevind(s, 17, 1) == 15
    @test prevind(s, 17, 2) == 14
    @test_throws BoundsError prevind(s, 18, 0)
    @test_throws BoundsError prevind(s, 18, 1)
    @test_throws BoundsError nextind(s, -1, 0)
    @test_throws BoundsError nextind(s, -1, 1)
    @test nextind(s, 0, 2) == 4
    @test nextind(s, 0, 20) == 26
    @test nextind(s, 0, 10) == 15
    @test nextind(s, 1, 1) == 4
    @test nextind(s, 1, 2) == 6
    @test nextind(s, 1, 9) == 15
    @test nextind(s, 1, 10) == 17
    @test nextind(s, 2, 1) == 4
    @test nextind(s, 3, 1) == 4
    @test nextind(s, 4, 1) == 6
    @test nextind(s, 14, 1) == 15
    @test nextind(s, 15, 1) == 17
    @test nextind(s, 15, 2) == 18
    @test nextind(s, 16, 1) == 17
    @test nextind(s, 16, 2) == 18
    @test nextind(s, 16, 3) == 19
    @test_throws BoundsError nextind(s, 17, 0)
    @test_throws BoundsError nextind(s, 17, 1)
    for k in 0:ncodeunits(s)+1
        n = p = k
        for j in 1:40
            if 1 ≤ p
                p = prevind(s, p)
                @test prevind(s, k, j) == p
            end
            if n ≤ ncodeunits(s)
                n = nextind(s, n)
                @test nextind(s, k, j) == n
            end
        end
    end
end

# julia#24839
@test permutedims([1 2; 3 4]) == [1 3; 2 4]
@test permutedims([1,2,3]) == [1 2 3]

# julia#27401
import Compat: ⋅
@test Compat.opnorm([1 2;3 4]) ≈ 5.464985704219043
@test Compat.opnorm([1 2;3 4], 1) ≈ 6
@test Compat.norm([1 2;3 4]) ≈ 5.477225575051661
@test Compat.norm([1 2;3 4], 1) ≈ 10
@test Compat.dot([1 2;3 4], [5 6;7 8]) == [1 2;3 4] ⋅ [5 6;7 8] ≈ 70

# 0.7.0-alpha.44
@test atan(1, 2) == atan(0.5)
@test atan(1.0, 2.0) == atan(0.5)
@test atan(-1.0, -2.0) ≈ atan(0.5) - π
@test atan(big"-1.0", big"-2.0") ≈ atan(big"0.5") - π

# 0.7.0-DEV.4724
let
    @test Compat.split("", ','  ; keepempty=false) == []
    @test Compat.split(",", ',' ; keepempty=false) == []
    @test Compat.split(",,", ','; keepempty=false) == []
    @test Compat.rsplit("", ','  ; keepempty=false) == []
    @test Compat.rsplit(",", ',' ; keepempty=false) == []
    @test Compat.rsplit(",,", ','; keepempty=false) == []

    str = "a.:.ba..:..cba.:.:.dcba.:."
    @test Compat.split(str, ".:."; keepempty=false) == ["a","ba.",".cba",":.dcba"]
    @test Compat.split(str, ".:."; keepempty=true) == ["a","ba.",".cba",":.dcba",""]
    @test Compat.split(str, ".:."; limit=3, keepempty=false) == ["a","ba.",".cba.:.:.dcba.:."]
    @test Compat.split(str, ".:."; limit=3, keepempty=true) == ["a","ba.",".cba.:.:.dcba.:."]
    @test Compat.rsplit(str, ".:."; keepempty=false) == ["a","ba.",".cba.:","dcba"]
    @test Compat.rsplit(str, ".:."; keepempty=true) == ["a","ba.",".cba.:","dcba",""]
    @test Compat.rsplit(str, ".:."; limit=3, keepempty=false) == ["a.:.ba.",".cba.:","dcba"]
    @test Compat.rsplit(str, ".:."; limit=3, keepempty=true) == ["a.:.ba..:..cba.:","dcba",""]

    @test Compat.split(str, r"\.(:\.)+"; keepempty=false) == ["a","ba.",".cba","dcba"]
    @test Compat.split(str, r"\.(:\.)+"; keepempty=true) == ["a","ba.",".cba","dcba",""]
    @test Compat.split(str, r"\.(:\.)+"; limit=3, keepempty=false) == ["a","ba.",".cba.:.:.dcba.:."]
    @test Compat.split(str, r"\.(:\.)+"; limit=3, keepempty=true) == ["a","ba.",".cba.:.:.dcba.:."]
    @test Compat.split(str, r"\.+:\.+"; keepempty=false) == ["a","ba","cba",":.dcba"]
    @test Compat.split(str, r"\.+:\.+"; keepempty=true) == ["a","ba","cba",":.dcba",""]
    @test Compat.split(str, r"\.+:\.+"; limit=3, keepempty=false) == ["a","ba","cba.:.:.dcba.:."]
    @test Compat.split(str, r"\.+:\.+"; limit=3, keepempty=true) == ["a","ba","cba.:.:.dcba.:."]
end

let
    # test required keyword arguments
    @compat func1() = 1
    @test func1() == 1 # using the function works
    @compat func2(x) = x
    @test func2(3) == 3 # using the function works
    @compat func3(;y) = y
    @test func3(y=2) == 2 # using the function works
    @test_throws UndefKeywordError func3()
    @compat func4(x; z) = x*z
    @test func4(2,z=3) == 6 # using the function works
    @test_throws UndefKeywordError func4(2)
    @compat func5(;x=1, y) = x*y
    @test func5(y=3) == 3
    @test func5(y=3, x=2) == 6
    @test_throws UndefKeywordError func5(x=2)
end

# 0.7.0-beta.73
let a = rand(5,5)
    s = mapslices(sort, a, dims=[1])
    S = mapslices(sort, a, dims=[2])
    for i = 1:5
        @test s[:,i] == sort(a[:,i])
        @test vec(S[i,:]) == sort(vec(a[i,:]))
    end
end

# 0.7.0-beta2.169
@test floatmin(Float16) == @eval $(Core.Intrinsics.bitcast(Float16, 0x0400))
@test floatmax(Float32) == @eval $(Core.Intrinsics.bitcast(Float32, 0x7f7fffff))
@test floatmin(zero(Float64)) == floatmin(Float64)

# 0.7.0-beta2.143
if VERSION < v"0.7.0-beta2.143"
    let a = reshape(Vector(1:4),(2,2,1,1)), b = reshape(Vector(1:4), (2,2,1))
        @test dropdims(a; dims=3) == b
        @test_throws UndefKeywordError dropdims(a)
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
