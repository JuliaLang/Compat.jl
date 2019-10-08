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

# 0.7.0-DEV.4804
@test Compat.trunc(pi) == 3.0
@test Compat.floor(pi) == 3.0
@test Compat.ceil(pi) == 4.0
@test Compat.round(pi) == 3.0
@test Compat.trunc(pi, digits = 3) == 3.141
@test Compat.floor(pi, digits = 3) == 3.141
@test Compat.ceil(pi, digits = 3) == 3.142
@test Compat.round(pi, digits = 3) == 3.142
@test Compat.round(pi, sigdigits = 5) == 3.1416
@test Compat.trunc(pi, base = 2) == 3.0
@test Compat.floor(pi, base = 2) == 3.0
@test Compat.ceil(pi, base = 2) == 4.0
@test Compat.round(pi, base = 2) == 3.0
@test Compat.trunc(pi, digits = 3, base = 2) == 3.125
@test Compat.floor(pi, digits = 3, base = 2) == 3.125
@test Compat.ceil(pi, digits = 3, base = 2) == 3.25
@test Compat.round(pi, digits = 3, base = 2) == 3.125
@test Compat.round(pi, sigdigits = 5, base = 2) == 3.125

# 0.7.0-DEV.3734
let buf = Compat.IOBuffer(read=true, write=false, maxsize=25)
    @test buf.readable
    @test !buf.writable
    @test buf.maxsize == 25
end
let buf = Compat.IOBuffer(zeros(UInt8, 4), write=true) # issue #502
    write(buf, 'a')
    @test take!(buf) == [0x61]
end
let buf = Compat.IOBuffer(sizehint=20)
    println(buf, "Hello world.")
    @test String(take!(buf)) == "Hello world.\n"
end

# 0.7.0-DEV.3986
@test_throws ArgumentError Compat.range(1)
@test_throws ArgumentError Compat.range(nothing)
@test_throws ArgumentError Compat.range(1, step=1)
@test_throws ArgumentError Compat.range(1, step=1, stop=4, length=3)
@test Compat.range(2, step=2, stop=8) == 2:2:8
@test Compat.range(2, stop=8) == 2:8
@test Compat.range(2, step=2, length=8) == 2:2:16
@test Compat.range(1.0, stop=2.0, length=3) == 1.0:0.5:2.0

# 0.7.0-DEV.3995
mktempdir(@__DIR__) do dir
    src = joinpath(dir, "src.jl")
    touch(src)
    dest = joinpath(dir, "dest.jl")
    touch(dest)
    open(src, "w") do f
        write(f, "Hello, world!")
    end
    Compat.cp(src, dest, force = true)
    open(dest, "r") do f
        @test read(f, String) == "Hello, world!"
    end
    Compat.mv(src, dest, force = true)
    open(dest, "r") do f
        @test read(f, String) == "Hello, world!"
    end
    @test readdir(dir) == ["dest.jl"]
end

# 0.7.0-DEV.3972
@test Compat.indexin([1, 2], [1, 0, 1]) == [1, nothing]

# 0.7.0-DEV.4585
@test isuppercase('A')
@test !isuppercase('a')
@test islowercase('a')
@test !islowercase('A')
@test uppercasefirst("qwerty") == "Qwerty"
@test lowercasefirst("Qwerty") == "qwerty"

# 0.7.0-DEV.4064
# some tests are behind a version check below because Julia gave
# the wrong result between 0.7.0-DEV.3262 and 0.7.0-DEV.4646
# see https://github.com/JuliaLang/julia/issues/26488
Issue26488 = VERSION < v"0.7.0-DEV.3262" || VERSION >= v"0.7.0-DEV.4646"
@test Compat.Statistics.mean([1 2; 3 4]) == 2.5
@test Compat.Statistics.mean([1 2; 3 4], dims=1) == [2 3]
@test Compat.Statistics.mean([1 2; 3 4], dims=2) == hcat([1.5; 3.5])
@test Compat.cumsum([1 2; 3 4], dims=1) == [1 2; 4 6]
@test Compat.cumsum([1 2; 3 4], dims=2) == [1 3; 3 7]
@test Compat.cumprod([1 2; 3 4], dims=1) == [1 2; 3 8]
@test Compat.cumprod([1 2; 3 4], dims=2) == [1 2; 3 12]
@test Compat.sum([1 2; 3 4]) == 10
@test Compat.sum([1 2; 3 4], dims=1) == [4 6]
@test Compat.sum([1 2; 3 4], dims=2) == hcat([3; 7])
@test Compat.sum(x -> x+1, [1 2; 3 4]) == 14
Issue26488 && @test Compat.sum(x -> x+1, [1 2; 3 4], dims=1) == [6 8]
Issue26488 && @test Compat.sum(x -> x+1, [1 2; 3 4], dims=2) == hcat([5; 9])
@test Compat.prod([1 2; 3 4]) == 24
@test Compat.prod([1 2; 3 4], dims=1) == [3 8]
@test Compat.prod([1 2; 3 4], dims=2) == hcat([2; 12])
@test Compat.prod(x -> x+1, [1 2; 3 4]) == 120
Issue26488 && @test Compat.prod(x -> x+1, [1 2; 3 4], dims=1) == [8 15]
Issue26488 && @test Compat.prod(x -> x+1, [1 2; 3 4], dims=2) == hcat([6; 20])
@test Compat.maximum([1 2; 3 4]) == 4
@test Compat.maximum([1 2; 3 4], dims=1) == [3 4]
@test Compat.maximum([1 2; 3 4], dims=2) == hcat([2; 4])
@test Compat.maximum(x -> x+1, [1 2; 3 4]) == 5
@test Compat.maximum(x -> x+1, [1 2; 3 4], dims=1) == [4 5]
@test Compat.maximum(x -> x+1, [1 2; 3 4], dims=2) == hcat([3; 5])
@test Compat.minimum([1 2; 3 4]) == 1
@test Compat.minimum([1 2; 3 4], dims=1) == [1 2]
@test Compat.minimum([1 2; 3 4], dims=2) == hcat([1; 3])
@test Compat.minimum(x -> x+1, [1 2; 3 4]) == 2
@test Compat.minimum(x -> x+1, [1 2; 3 4], dims=1) == [2 3]
@test Compat.minimum(x -> x+1, [1 2; 3 4], dims=2) == hcat([2; 4])
@test Compat.all([true false; true false]) == false
@test Compat.all([true false; true false], dims=1) == [true false]
@test Compat.all([true false; true false], dims=2) == hcat([false; false])
@test Compat.all(isodd, [1 2; 3 4]) == false
@test Compat.all(isodd, [1 2; 3 4], dims=1) == [true false]
@test Compat.all(isodd, [1 2; 3 4], dims=2) == hcat([false; false])
@test Compat.any([true false; true false]) == true
@test Compat.any([true false; true false], dims=1) == [true false]
@test Compat.any([true false; true false], dims=2) == hcat([true; true])
@test Compat.any(isodd, [1 2; 3 4]) == true
@test Compat.any(isodd, [1 2; 3 4], dims=1) == [true false]
@test Compat.any(isodd, [1 2; 3 4], dims=2) == hcat([true; true])
@test Compat.findmax([3, 2, 7, 4]) == (7, 3)
@test Compat.findmax([3, 2, 7, 4], dims=1) == ([7], [3])
@test Compat.findmax([1 2; 3 4], dims=1) == ([3 4], [CartesianIndex(2, 1) CartesianIndex(2, 2)])
@test Compat.findmax([1 2; 3 4]) == (4, CartesianIndex(2, 2))
@test Compat.findmax([1 2; 3 4], dims=1) == ([3 4], [CartesianIndex(2, 1) CartesianIndex(2, 2)])
@test Compat.findmax([1 2; 3 4], dims=2) == (hcat([2; 4]), hcat([CartesianIndex(1, 2); CartesianIndex(2, 2)]))
@test Compat.findmin([3, 2, 7, 4]) == (2, 2)
@test Compat.findmin([3, 2, 7, 4], dims=1) == ([2], [2])
@test Compat.findmin([1 2; 3 4]) == (1, CartesianIndex(1, 1))
@test Compat.findmin([1 2; 3 4], dims=1) == ([1 2], [CartesianIndex(1, 1) CartesianIndex(1, 2)])
@test Compat.findmin([1 2; 3 4], dims=2) == (hcat([1; 3]), hcat([CartesianIndex(1, 1); CartesianIndex(2, 1)]))
if VERSION  < v"0.7.0-DEV.5238"
    # Test these functions if their counterparts are defined in Base. In the future, this
    # will be dealt with in StatsBase
    @test Compat.Statistics.varm([1 2; 3 4], -1) == 18
    @test Compat.Statistics.varm([1 2; 3 4], [-1 -2], dims=1) == [20 52]
    @test Compat.Statistics.varm([1 2; 3 4], [-1, -2], dims=2) == hcat([13, 61])
    @test Compat.Statistics.var([1 2; 3 4]) == 5/3
    @test Compat.Statistics.var([1 2; 3 4], dims=1) == [2 2]
    @test Compat.Statistics.var([1 2; 3 4], dims=2) == hcat([0.5, 0.5])
    @test Compat.Statistics.var([1 2; 3 4], corrected=false) == 1.25
    @test Compat.Statistics.var([1 2; 3 4], corrected=false, dims=1) == [1 1]
    @test Compat.Statistics.var([1 2; 3 4], corrected=false, dims=2) == hcat([0.25, 0.25])
    @test Compat.Statistics.std([1 2; 3 4]) == sqrt(5/3)
    @test Compat.Statistics.std([1 2; 3 4], dims=1) == [sqrt(2) sqrt(2)]
    @test Compat.Statistics.std([1 2; 3 4], dims=2) == hcat([sqrt(0.5), sqrt(0.5)])
    @test Compat.Statistics.std([1 2; 3 4], corrected=false) == sqrt(1.25)
    @test Compat.Statistics.std([1 2; 3 4], corrected=false, dims=1) == [sqrt(1) sqrt(1)]
    @test Compat.Statistics.std([1 2; 3 4], corrected=false, dims=2) == hcat([sqrt(0.25), sqrt(0.25)])
    @test Compat.Statistics.cov([1 2; 3 4]) == [2 2; 2 2]
    @test Compat.Statistics.cov([1 2; 3 4], dims=1) == [2 2; 2 2]
    @test Compat.Statistics.cov([1 2; 3 4], dims=2) == [0.5 0.5; 0.5 0.5]
    @test Compat.Statistics.cov([1 2; 3 4], [4; 5]) == hcat([1; 1])
    @test Compat.Statistics.cov([1 2; 3 4], [4; 5], dims=1) == hcat([1; 1])
    @test Compat.Statistics.cov([1 2; 3 4], [4; 5], dims=2) == hcat([0.5; 0.5])
    @test Compat.Statistics.cov([1 2; 3 4], [4; 5], corrected=false) == hcat([0.5; 0.5])
    @test Compat.Statistics.cov([1 2; 3 4], [4; 5], corrected=false, dims=1) == hcat([0.5; 0.5])
    @test Compat.Statistics.cov([1 2; 3 4], [4; 5], corrected=false, dims=2) == hcat([0.25; 0.25])
    @test Compat.Statistics.cor([1 2; 3 4]) ≈ [1 1; 1 1]
    @test Compat.Statistics.cor([1 2; 3 4], dims=1) ≈ [1 1; 1 1]
    @test Compat.Statistics.cor([1 2; 3 4], dims=2) ≈ [1 1; 1 1]
    @test Compat.Statistics.cor([1 2; 3 4], [4; 5]) ≈ [1; 1]
    @test Compat.Statistics.cor([1 2; 3 4], [4; 5], dims=1) ≈ [1; 1]
    @test Compat.Statistics.cor([1 2; 3 4], [4; 5], dims=2) ≈ [1; 1]
end
@test Compat.Statistics.median([1 2; 3 4]) == 2.5
@test Compat.Statistics.median([1 2; 3 4], dims=1) == [2 3]
@test Compat.Statistics.median([1 2; 3 4], dims=2) == hcat([1.5; 3.5])
@test Compat.mapreduce(string, *, [1 2; 3 4]) == "1324"
Issue26488 && @test Compat.mapreduce(string, *, [1 2; 3 4], dims=1) == ["13" "24"]
Issue26488 && @test Compat.mapreduce(string, *, [1 2; 3 4], dims=2) == hcat(["12", "34"])
@test Compat.mapreduce(string, *, [1 2; 3 4], init="z") == "z1324"
@test Compat.mapreduce(string, *, (1, 2, 3, 4), init="z") == "z1234"
@test Compat.mapreduce(string, *, [1 2; 3 4], dims=1, init="z") == ["z13" "z24"]
@test Compat.mapreduce(string, *, [1 2; 3 4], dims=2, init="z") == hcat(["z12", "z34"])
@test Compat.reduce(*, [1 2; 3 4]) == 24
@test Compat.reduce(*, [1 2; 3 4], dims=1) == [3 8]
@test Compat.reduce(*, [1 2; 3 4], dims=2) == hcat([2, 12])
@test Compat.reduce(*, [1 2; 3 4], init=10) == 240
@test Compat.reduce(*, (1, 2, 3, 4), init=10) == 240
@test Compat.reduce(*, [1 2; 3 4], dims=1, init=10) == [30 80]
@test Compat.reduce(*, [1 2; 3 4], dims=2, init=10) == hcat([20, 120])
@test Compat.sort([1, 2, 3, 4]) == [1, 2, 3, 4]
@test Compat.sort([1 2; 3 4], dims=1) == [1 2; 3 4]
@test Compat.sort([1 2; 3 4], dims=2) == [1 2; 3 4]
@test Compat.sort([1, 2, 3, 4], rev=true) == [4, 3, 2, 1]
@test Compat.sort([1 2; 3 4], rev=true, dims=1) == [3 4; 1 2]
@test Compat.sort([1 2; 3 4], rev=true, dims=2) == [2 1; 4 3]
@test Compat.accumulate(*, [1 2; 3 4], dims=1) == [1 2; 3 8]
@test Compat.accumulate(*, [1 2; 3 4], dims=2) == [1 2; 3 12]
@test Compat.cumsum([1 2; 3 4], dims=1) == [1 2; 4 6]
@test Compat.cumsum([1 2; 3 4], dims=2) == [1 3; 3 7]
@test Compat.cumprod([1 2; 3 4], dims=1) == [1 2; 3 8]
@test Compat.cumprod([1 2; 3 4], dims=2) == [1 2; 3 12]
let b = zeros(2,2)
    Compat.accumulate!(*, b, [1 2; 3 4], dims=1)
    @test b == [1 2; 3 8]
    Compat.accumulate!(*, b, [1 2; 3 4], dims=2)
    @test b == [1 2; 3 12]
    Compat.cumsum!(b, [1 2; 3 4], dims=1)
    @test b == [1 2; 4 6]
    Compat.cumsum!(b, [1 2; 3 4], dims=2)
    @test b == [1 3; 3 7]
    Compat.cumprod!(b, [1 2; 3 4], dims=1)
    @test b == [1 2; 3 8]
    Compat.cumprod!(b, [1 2; 3 4], dims=2)
    @test b == [1 2; 3 12]
end
@test Compat.reverse([1, 2, 3, 4]) == [4, 3, 2, 1]
@test Compat.reverse([1 2; 3 4], dims=1) == [3 4; 1 2]
@test Compat.reverse([1 2; 3 4], dims=2) == [2 1; 4 3]

# 0.7.0-DEV.4738
if VERSION < v"0.7.0-beta2.143"
    @test squeeze([1 2], dims=1) == [1, 2]
    @test_throws ArgumentError squeeze([1 2], dims=2)
    @test_throws ArgumentError squeeze(hcat([1, 2]), dims=1)
    @test squeeze(hcat([1, 2]), dims=2) == [1, 2]
    @test_throws Exception squeeze([1,2])
end

# 0.7.0-DEV.5165
@test Compat.cat([1, 2], [3, 4, 5], dims = 1) == [1, 2, 3, 4, 5]
@test Compat.cat([1, 2], [3, 4], dims = 2) == [1 3; 2 4]
if VERSION < v"0.7.0-DEV.5165"
    @test_throws UndefKeywordError Compat.cat([1, 2], [3, 4])
end

# 0.7.0-DEV.3976
let A = rand(5,5)
    @test selectdim(A, 1, 3) == A[3, :]
    @test selectdim(A, 1, 1:3) == A[1:3, :]
    @test selectdim(A, 2, 3) == A[:, 3]
    @test selectdim(A, 2, 1:3) == A[:, 1:3]
    selectdim(A, 1, 3)[3] = 42
    @test A[3,3] == 42
    if VERSION < v"0.7.0-DEV.3976" || VERSION >= v"0.7.0-DEV.4739"
        # in the omitted version range, Julia's selectdim always gives IndexCartesian()
        B = rand(4, 3, 2)
        @test IndexStyle(selectdim(B, 1, 1)) == IndexStyle(view(B, 1, :, :)) == IndexLinear()
        @test IndexStyle(selectdim(B, 2, 1)) == IndexStyle(view(B, :, 1, :)) == IndexCartesian()
        @test IndexStyle(selectdim(B, 3, 1)) == IndexStyle(view(B, :, :, 1)) == IndexLinear()
    end
end

# 0.7.0-DEV.843 / 0.7.0-DEV.2337
let A = [1 2; 1 2; 1 2]
    if VERSION < v"0.7.0-DEV.5211"
        # the full keyword was only temporarily available in Base, so these  methods don't
        # work on 0.7 anymore, but we test them for the time being to avoid accidentally
        # breaking anyone's code
        f = Compat.qr(A, Val(false), full=false)
        @test f == Compat.qr(A, Val(false))
        @test length(f) == 2
        @test size(f[1]) == (3, 2)
        @test f[1] * f[2] ≈ A
        f = Compat.qr(A, Val(false), full=true)
        @test length(f) == 2
        @test size(f[1]) == (3, 3)
        @test f[1] * [f[2]; [0 0]] ≈ A
        f = Compat.qr(A, Val(true), full=false)
        @test f == Compat.qr(A, Val(true))
        @test length(f) == 3
        @test size(f[1]) == (3, 2)
        @test f[1] * f[2] ≈ A[:,f[3]]
        f = Compat.qr(A, Val(true), full=true)
        @test length(f) == 3
        @test size(f[1]) == (3, 3)
        @test f[1] * [f[2]; [0 0]] ≈ A[:,f[3]]
    else
        f = Compat.qr(A, Val(false))
        @test size(f.Q) == (3, 3)
        @test f.Q * [f.R; [0 0]] ≈ A
        f = Compat.qr(A, Val(true))
        @test size(f.Q) == (3, 3)
        @test f.Q * [f.R; [0 0]] ≈ A[:,f.p]
    end
end

let A = [1 2; 3 4]
    @test Compat.rmul!(A, 2) == [2 4; 6 8]
    @test Compat.rmul!(A, Diagonal([1, 2])) == [2 8; 6 16]
    @test Compat.rmul!(A, UpperTriangular([2 2; 3 3])) == [4 28; 12 60]
    @test Compat.rmul!(LowerTriangular(A), Diagonal([1, 2])) == LowerTriangular([4 0; 12 120])
    @test Compat.rmul!(Diagonal(A), Diagonal([2, 1])) == Diagonal([8, 120])
end

# 0.7.0-DEV.3936
@test let ct = current_task(), t = @task true
    schedule(ct)
    yieldto(t)
    fetch(t)
end

# 0.7.0-DEV.5087
@test isletter('a')
@test isletter('β')
@test !isletter('3')

# 0.7.0-DEV.4905
@test isbitstype(Int)
@test !isbitstype(Vector{Int})

# 0.7.0-DEV.4762
let ptr = @cfunction(+, Int, (Int, Int))
    @test ptr isa Ptr{Cvoid}
    @test ptr != C_NULL
    @test ccall(ptr, Int, (Int, Int), 2, 3) == 5
end
# issue #565
issue565(x) = x + 1
const Issue565 = Int
let bar() = @cfunction(issue565, Issue565, (Issue565,)), ptr = bar()
    @test ptr isa Ptr{Cvoid}
    @test ptr != C_NULL
    @test ccall(ptr, Int, (Int,), 2) === 3
end

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

@test repeat([1, 2], 3) == [1, 2, 1, 2, 1, 2]
@test repeat(1:4, 2) == [1, 2, 3, 4, 1, 2, 3, 4]
@test repeat([1 2; 3 4], 2, 3) == [1  2  1  2  1  2
                                   3  4  3  4  3  4
                                   1  2  1  2  1  2
                                   3  4  3  4  3  4]
@test repeat([1, 2], 1, 2, 3) == [x for x in 1:2, y in 1:2, z in 1:3]

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
