using Compat
using Compat.Test
using Compat.LinearAlgebra
using Compat.SparseArrays

@test isempty(detect_ambiguities(Base, Core, Compat))

const struct_sym = VERSION < v"0.7.0-DEV.1263" ? :type : :struct

for os in [:apple, :bsd, :linux, :unix, :windows]
    from_base = if VERSION >= v"0.7.0-DEV.914"
        Expr(:., Expr(:., :Base, Base.Meta.quot(:Sys)), Base.Meta.quot(Symbol("is", os)))
    else
        Expr(:., :Base, Base.Meta.quot(Symbol("is_", os)))
    end
    @eval @test Compat.Sys.$(Symbol("is", os))() == $from_base()
end

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

# julia#26365
@test Compat.tr([1 2; 3 5]) == 6

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

# julia#20006
abstract type AbstractFoo20006 end
eval(Expr(
    struct_sym, false,
    Expr(:(<:), :(ConcreteFoo20006{T<:Int}), :AbstractFoo20006),
    quote end))
eval(Expr(
    struct_sym, false,
    Expr(:(<:), :(ConcreteFoo20006N{T<:Int,N}), :AbstractFoo20006),
    quote end))
@compat ConcreteFoo200061{T<:Int} = ConcreteFoo20006N{T,1}
@test Compat.TypeUtils.isabstract(AbstractFoo20006)
@test !Compat.TypeUtils.isabstract(ConcreteFoo20006)
@test !Compat.TypeUtils.isabstract(ConcreteFoo20006N)
@test !Compat.TypeUtils.isabstract(ConcreteFoo200061)
@test !Compat.TypeUtils.isabstract(StridedArray)
@test Compat.TypeUtils.parameter_upper_bound(ConcreteFoo20006, 1) == Int
@test isa(Compat.TypeUtils.typename(Array), Core.TypeName)
@test isabstracttype(AbstractFoo20006)
@test !isabstracttype(ConcreteFoo20006)
@test !isabstracttype(ConcreteFoo20006N)
@test !isabstracttype(ConcreteFoo200061)
@test !isabstracttype(StridedArray)

# PR 20203
@test Compat.readline(IOBuffer("Hello, World!\n")) == "Hello, World!"
@test Compat.readline(IOBuffer("x\n"), keep=false) == "x"
@test Compat.readline(IOBuffer("x\n"), keep=true) == "x\n"
@test collect(Compat.eachline(IOBuffer("x\ny"))) == ["x", "y"]
@test collect(Compat.eachline(IOBuffer("x\ny"), keep=false)) == ["x", "y"]
@test collect(Compat.eachline(IOBuffer("x\ny"), keep=true))  == ["x\n", "y"]

# PR 25646
for (t, s, m, kept) in [
        ("a", "ab", "a", "a"),
        ("b", "ab", "b", "b"),
        ("α", "αγ", "α", "α"),
        ("ab", "abc", "ab", "ab"),
        ("bc", "abc", "bc", "bc"),
        ("αβ", "αβγ", "αβ", "αβ"),
        ("aaabc", "ab", "aa", "aaab"),
        ("aaabc", "ac", "aaabc", "aaabc"),
        ("aaabc", "aab", "a", "aaab"),
        ("aaabc", "aac", "aaabc", "aaabc"),
        ("αααβγ", "αβ", "αα", "αααβ"),
        ("αααβγ", "ααβ", "α", "αααβ"),
        ("αααβγ", "αγ", "αααβγ", "αααβγ"),
        ("barbarbarians", "barbarian", "bar", "barbarbarian"),
        ("abcaabcaabcxl", "abcaabcx", "abca", "abcaabcaabcx"),
        ("abbaabbaabbabbaax", "abbaabbabbaax", "abba", "abbaabbaabbabbaax"),
        ("abbaabbabbaabbaabbabbaax", "abbaabbabbaax", "abbaabbabba", "abbaabbabbaabbaabbabbaax"),
       ]
    local t, s, m, kept
    @test Compat.readuntil(IOBuffer(t), s) == m
    @test Compat.readuntil(IOBuffer(t), s, keep=true) == kept
    @test Compat.readuntil(IOBuffer(t), SubString(s, firstindex(s))) == m
    @test Compat.readuntil(IOBuffer(t), SubString(s, firstindex(s)), keep=true) == kept
    @test Compat.readuntil(IOBuffer(t), GenericString(s)) == m
    @test Compat.readuntil(IOBuffer(t), GenericString(s), keep=true) == kept
    @test Compat.readuntil(IOBuffer(t), Vector{UInt8}(codeunits(s))) == Vector{UInt8}(codeunits(m))
    @test Compat.readuntil(IOBuffer(t), Vector{UInt8}(codeunits(s)), keep=true) == Vector{UInt8}(codeunits(kept))
    @test Compat.readuntil(IOBuffer(t), collect(s)::Vector{Char}) == Vector{Char}(m)
    @test Compat.readuntil(IOBuffer(t), collect(s)::Vector{Char}, keep=true) == Vector{Char}(kept)
end

# PR 22064
module Test22064
using Compat
using Compat.Test
@test (@__MODULE__) === Test22064
end

# invokelatest with keywords
pr22646(x; y=0) = 1
let foo() = begin
        eval(:(pr22646(x::Int; y=0) = 2))
        return Compat.invokelatest(pr22646, 0, y=1)
    end
    @test foo() == 2
end

# PR #21197
let c = `ls -l "foo bar"`
    @test collect(c) == ["ls", "-l", "foo bar"]
    @test first(c) == "ls" == c[1]
    @test last(c) == "foo bar" == c[3] == c[end]
    @test c[1:2] == ["ls", "-l"]
    @test eltype(c) == String
    @test length(c) == 3
    @test eachindex(c) == 1:3
end

# PR 22629
@test logdet(0.5) == log(det(0.5))

# PR 22633
if VERSION < v"0.7.0-DEV.5272"
    # chol(A::UniformScaling) has been deprecated in Julia, we still test it to avoid
    # accidental breakage in packages using the Compat vesion of it on Julia 0.6
    for T in (Float64, ComplexF32, BigFloat, Int)
        λ = T(4)
        @test chol(λ*I).λ ≈ √λ
        @test_throws Union{ArgumentError,Compat.LinearAlgebra.PosDefException} chol(-λ*I)
    end
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

# PR 22350
eval(Expr(struct_sym, false, :TestType, Expr(:block, :(a::Int), :b)))
@test fieldcount(TestType) == 2
@test fieldcount(Int) == 0

# PR 20005
@test_throws InexactError throw(InexactError(:func, Int, 3.2))

# PR 22751
@test_throws DomainError throw(DomainError(-2))
@test_throws DomainError throw(DomainError(-2, "negative"))

# PR 22761
@test_throws OverflowError throw(OverflowError("overflow"))

# PR 22907
using Compat: pairs

# keys, values, pairs
for A in (rand(2), rand(2,3))
    local A
    for (i, v) in pairs(A)
        @test A[i] == v
    end
    @test collect(values(A)) == collect(A)
end

let A = Dict(:foo=>1, :bar=>3)
    for (k, v) in pairs(A)
        @test A[k] == v
    end
    @test sort!(collect(pairs(A))) == sort!(collect(A))
end

let
    A14 = [11 13; 12 14]
    R = CartesianIndices(Compat.axes(A14))
    @test vec([a for (a,b) in pairs(IndexLinear(),    A14)]) == [1,2,3,4]
    @test vec([a for (a,b) in pairs(IndexCartesian(), A14)]) == vec(collect(R))
    @test vec([b for (a,b) in pairs(IndexLinear(),    A14)]) == [11,12,13,14]
    @test vec([b for (a,b) in pairs(IndexCartesian(), A14)]) == [11,12,13,14]
end

# Val(x)
# 0.7
begin
    local firstlast
    firstlast(::Val{true}) = "First"
    firstlast(::Val{false}) = "Last"

    @test firstlast(Val(true)) == "First"
    @test firstlast(Val(false)) == "Last"
end

# Reshape to a given number of dimensions using Val(N)
# 0.7
let
    for A in (rand(Float64, ()), rand(2), rand(2,3), rand(2,3,5), rand(2,3,5,7)), N in (1,2,3,4,5,6)
        B = @inferred reshape(A, Val(N))
        @test ndims(B) == N
        if N < ndims(A)
            new_sz = (size(A)[1:N-1]..., prod(size(A)[N:end]))
        elseif N == ndims(A)
            new_sz = size(A)
        else
            new_sz = (size(A)..., ntuple(x->1, N-ndims(A))...)
        end
        @test size(B) == new_sz
        @test B == reshape(A, new_sz)
    end
end

# ntuple with Val(N)
# 0.7
@test @inferred(ntuple(x->1, Val(3))) == (1,1,1)
@test @inferred(ntuple(x->x, Val(0))) == ()
@test @inferred(ntuple(x->x, Val(5))) == (1,2,3,4,5)

# @nospecialize
# 0.7
no_specialize(@nospecialize(x)) = sin(1)
no_specialize(@nospecialize(x::Integer)) = sin(2)
@test no_specialize(1.0) == sin(1)
@test no_specialize(1) == sin(2)
no_specialize_kw1(@nospecialize(x=0)) = sin(1)
no_specialize_kw1(@nospecialize(x::Integer)) = sin(2)
@test no_specialize_kw1(1.0) == sin(1)
@test no_specialize_kw1(1) == sin(2)
@test no_specialize_kw1() == sin(2)
no_specialize_kw2(@nospecialize(x)) = sin(1)
no_specialize_kw2(@nospecialize(x::Integer=0)) = sin(2)
@test no_specialize_kw2(1.0) == sin(1)
@test no_specialize_kw2(1) == sin(2)
@test no_specialize_kw2() == sin(2)

# 0.7
@test read(IOBuffer("aaaa"), String) == "aaaa"
@test occursin("read(@__FILE__, String)", read(@__FILE__, String))
let cmd = `$(Base.julia_cmd()) --startup-file=no -e "println(:aaaa)"`
    @test read(cmd, String) == "aaaa\n"
    @test read(pipeline(cmd, stderr=devnull), String) == "aaaa\n"
end

# 0.7
@test isa(1:2, AbstractRange)

# 0.7
let M = [1 + 2im 3 + 4im; 5 + 6im 7 + 8im],
    M2 = adjoint(copy(M)),
    Mc = [1 - 2im 5 - 6im; 3 - 4im 7 - 8im]

    @test adjoint(M) == Mc
    M2 .= 0
    adjoint!(M2, M)
    @test M2 == Mc
end

# 0.7
module TestMathConstants
using Compat.MathConstants
end
for name in [:π, :pi, :ℯ, :e, :γ, :eulergamma, :catalan, :φ, :golden]
    @test isdefined(TestMathConstants, name) && !Base.isdeprecated(TestMathConstants, name)
    @test isdefined(Compat.MathConstants, name) && !Base.isdeprecated(Compat.MathConstants, name)
end
module TestMathConstants2
using Compat
end
@test isdefined(TestMathConstants2, :ℯ) && !Base.isdeprecated(TestMathConstants, :ℯ)

# 0.7
@test partialsort([3,6,30,1,9], 2, rev=true) == 9
@test partialsort([3,6,30,1,9], 2, by=x->1/x) == 9
@test partialsortperm([3,6,30,1,9], 2, rev=true) == 5
@test partialsortperm([3,6,30,1,9], 2, by=x->1/x) == 5

# 0.7
@test isa(Base.rtoldefault(1.0, 2.0, 0), Float64)
@test isa(Base.rtoldefault(Float64, 2.0, 0), Float64)
@test isa(Base.rtoldefault(1.0, Float64, 0), Float64)
@test isa(Base.rtoldefault(Float64, Float64, 0), Float64)
@test Base.rtoldefault(Float64, Float64, 1.0) === 0.0

# 0.7
if VERSION  < v"0.7.0-DEV.5238"
    # Test the extended cov if cov is part of Base. In the future, this will be dealt with
    # in StatsBase
    @test cov([1 2; 3 4], 1, corrected=true) == fill(2.0, 2, 2)
    @test cov([1 2; 3 4], 1, corrected=false) == fill(1.0, 2, 2)
    @test cov([1 2; 3 4], [0 4; 8 9], 1, corrected=true) == [8.0 5.0; 8.0 5.0]
    @test cov([1 2; 3 4], [0 4; 8 9], 1, corrected=false) == [4.0 2.5; 4.0 2.5]
    @test cov([1, 2], corrected=true) === 0.5
    @test cov([1, 2], corrected=false) === 0.25
    @test cov([1, 2], [0, 10], corrected=true) === 5.0
    @test cov([1, 2], [0, 10], corrected=false) === 2.5
end

# 0.7
@test isconcretetype(Int)

# 0.7
module Test23876
    using Compat
    using Compat.Test
    import Compat.DelimitedFiles
    using Compat.Mmap, Compat.SharedArrays
    using Compat.Distributed
    @test isdefined(@__MODULE__, :DelimitedFiles)
    @test isdefined(SharedArrays, :SharedArray)
    @test isdefined(@__MODULE__, :SharedArray)
    @test isdefined(@__MODULE__, :procs)
    @test isdefined(@__MODULE__, :remote_do)
    @test isdefined(Mmap, :mmap)
end

# 0.7
module Test24459
    using Compat
    using Compat.Test
    using Compat.Dates
    @test isdefined(@__MODULE__, :Dates)
end

# 0.7
module Test25056
    using Compat
    using Compat.Test
    using Compat.Printf
    @test isdefined(@__MODULE__, :Printf)
    @test isdefined(@__MODULE__, Symbol("@printf"))
    @test isdefined(@__MODULE__, Symbol("@sprintf"))
end

# 0.7

let a = [0,1,2,3,0,1,2,3]
    # curried isequal
    @test findfirst(isequal(3), [1,2,4,1,2,3,4]) == 6
    @test findfirst(!isequal(1), [1,2,4,1,2,3,4]) == 2
    @test findnext(isequal(1), a, 4) == 6
    # @test findnext(isequal(5), a, 4) == 0
    @test findlast(isequal(3), [1,2,4,1,2,3,4]) == 6
    @test findprev(isequal(1), a, 4) == 2
    @test findprev(isequal(1), a, 8) == 6
    if VERSION < v"0.7.0-DEV.4592"
        # test that equalto work on 0.6
        @test findfirst(equalto(3), [1,2,4,1,2,3,4]) == 6
        @test findfirst(!equalto(1), [1,2,4,1,2,3,4]) == 2
    end
    # curried ==
    @test findfirst(==(3), [1,2,4,1,2,3,4]) == 6
    @test findfirst(!(==(1)), [1,2,4,1,2,3,4]) == 2
    @test findnext(==(1), a, 4) == 6
    @test findlast(==(3), [1,2,4,1,2,3,4]) == 6
    @test findprev(==(1), a, 4) == 2
    @test findprev(==(1), a, 8) == 6
end

# 0.7
@test 'a'*"b" == "a"*'b' == 'a'*'b' == "ab"

# 0.7
@test 1 in BitSet(1:10)

# 0.7.0-DEV.1930
@test textwidth("A") == 1
@test textwidth('A') == 1

# 0.7
@test diagm(0 => ones(2), -1 => ones(2)) == [1.0 0.0 0.0; 1.0 1.0 0.0; 0.0 1.0 0.0]
@test diagm(0 => ones(2), 1 => ones(2)) == [1.0 1.0 0.0; 0.0 1.0 1.0; 0.0 0.0 0.0]
@test spdiagm(0 => ones(2), -1 => ones(2)) == [1.0 0.0 0.0; 1.0 1.0 0.0; 0.0 1.0 0.0]
@test spdiagm(0 => ones(2), 1 => ones(2)) == [1.0 1.0 0.0; 0.0 1.0 1.0; 0.0 0.0 0.0]

# 0.7
let a = [1 0 0; 0 1 0; 0 0 1]
    @test Matrix{Int}(I, 3, 3)::Matrix{Int} == a
    @test Matrix{Float64}(I, (3, 2))::Matrix{Float64} == a[:,1:2]
    @test Array{Int}(I, (3, 3))::Matrix{Int} == a
    @test Array{Float64}(I, 3, 2)::Matrix{Float64} == a[:,1:2]
    @test SparseMatrixCSC{Int}(I, 3, 3)::SparseMatrixCSC{Int,Int} == a
    @test SparseMatrixCSC{Float64}(I, (3, 2))::SparseMatrixCSC{Float64,Int} == a[:,1:2]
    @test SparseMatrixCSC{Bool,Int16}(I, (3, 3))::SparseMatrixCSC{Bool,Int16} == a
    @test SparseMatrixCSC{ComplexF64,Int8}(I, 3, 2)::SparseMatrixCSC{ComplexF64,Int8} == a[:,1:2]

    @test Matrix(2I, 3, 3)::Matrix{Int} == Matrix(2I, (3, 3))::Matrix{Int} == 2a
    @test Matrix(2.0I, 3, 3)::Matrix{Float64} == Matrix(2.0I, (3, 3))::Matrix{Float64} == 2a
end

# 0.7.0-DEV.2581, 0.7.0-DEV.4527
@test isa(Vector(undef, 2), Vector{Any})
@test isa(Vector{Float64}(undef, 2), Vector{Float64})
@test isa(Matrix(undef, 2, 2), Matrix{Any})
@test isa(Matrix{Float64}(undef, 2, 2), Matrix{Float64})
@test isa(Array{Float64}(undef, 2, 2), Matrix{Float64})
@test isa(Array{Float64,3}(undef, 2, 2, 2), Array{Float64,3})

# 0.7.0-DEV.2687, 0.7.0-DEV.4527
@test isa(BitVector(undef, 2), BitVector)
@test isa(BitArray(undef, 2, 2), BitMatrix)

# 0.7.0-DEV.1472
@test get(IOContext(IOBuffer(), :arg1=>true, :arg2=>true, :arg3=>true), :arg3, false)
@test get(IOContext(IOBuffer(), :arg1=>true, :arg2=>true), :arg2, false)

# 0.7.0-DEV.2338
module Test24361
    using Compat
    using Compat.Test
    @test String(Compat.Base64.base64decode("SGVsbG8h")) == "Hello!"
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

# 0.7.0-DEV.1499
let key = "TEST_23412"
    @test !haskey(ENV, key)
    @test get(() -> "default", ENV, key) == "default"
end

# 0.7.0-DEV.2919
@test ComplexF16 === Complex{Float16}
@test ComplexF32 === Complex{Float32}
@test ComplexF64 === Complex{Float64}

# 0.7.0-DEV.3073
if VERSION < v"0.7.0-DEV.3073"
    @test Compat.Sys.BINDIR == JULIA_HOME
else
    @test Compat.Sys.BINDIR == Sys.BINDIR
end

# 0.7.0-DEV.2915
module Test25021
    using Compat
    using Compat.Test
    using Compat.Unicode
    @test isdefined(@__MODULE__, :Unicode)

    @test !isnumeric('a')
    @test isnumeric('1')
    @test titlecase("firstname lastname") == "Firstname Lastname"
    @test Compat.Unicode.isassigned('柒') && !Compat.Unicode.isassigned(0xfffe)
    @test Compat.Unicode.normalize("\U1e9b\U0323", :NFKC) == "\U1e69"
    @test Compat.Unicode.normalize("\t\r", stripcc=true) == "  "
end

# 0.7.0-DEV.3393
@test !isnumeric('a')
@test isnumeric('1')

# 0.7.0-DEV.2951
@test AbstractDict === (isdefined(Base, :AbstractDict) ? Base.AbstractDict : Base.Associative)

# 0.7.0-DEV.2978
@test Compat.axes === (isdefined(Base, :axes) ? Base.axes : Base.indices)
@test Compat.axes(1) == ()
@test Compat.axes(1,1) == 1:1

# 0.7.0-DEV.3137
@test Nothing === (isdefined(Base, :Nothing) ? Base.Nothing : Base.Void)
@test Nothing === Cvoid

# 0.7.0-DEV.3017
@test isa(Some(1), Some{Int})
@test convert(Some{Float64}, Some(1)) == Some(1.0)
@test convert(Nothing, nothing) == nothing
@test_throws MethodError convert(Nothing, 1)
@test Some(nothing) != nothing
if VERSION < v"0.7.0-DEV.5278"
    # coalesce has changed; old behavior kept and tested to avoid accidental breakage
    @test coalesce(Some(1)) == 1
    @test coalesce(nothing) == nothing
    @test coalesce(nothing, Some(1), Some(2)) == 1
end
@test Compat.notnothing(1) == 1
@test_throws ArgumentError Compat.notnothing(nothing)

# 0.7.0-DEV.3155
let coolvec = [1,2,3]
    @test pushfirst!(coolvec, 0) == [0,1,2,3]
    @test popfirst!(coolvec) == 0
end

# 0.7.0-DEV.3309
let v = [1, 2, 3]
    @test Compat.IteratorSize(v) isa Base.HasShape
    @test Compat.IteratorEltype(v) == Base.HasEltype()
end

# 0.7.0-DEV.3057
let A = [0, 0, 0], B = [1, 2, 3]
    @test copyto!(A, B) === A == B
end
let A = [0, 0, 0], B = [1, 2, 3]
    @test unsafe_copyto!(A, 2, B, 1, 1) === A == [0, 1, 0]
end

# 0.7.0-DEV.3406
using Compat.Random
@test rand(MersenneTwister(1234)) == 0.5908446386657102

# 0.7, make sure this works on 0.6
if VERSION < v"0.7.0-DEV.3272"
    @test contains("Hello, World!", r"World")
end

# 0.7.0-DEV.4639
@test occursin(r"World", "Hello, World!")
@test occursin(r"World", "Hello, World!", offset = 4)
@test occursin("World", "Hello, World!")
# 0.7.0-DEV.912
@test occursin('W', "Hello, World!")

# 0.7.0-DEV.3449
let A = [2.0 1.0; 1.0 3.0], b = [2.0, 3.0]
    @test diag(A) == b
end

# 0.7.0-DEV.3173
@test invpermute!(permute!([1, 2], 2:-1:1), 2:-1:1) == [1, 2]

# 0.7.0-DEV.3172
@test replace("abcb", "b"=>"c") == "accc"
@test replace("abcb", "b"=>"c", count=1) == "accb"
# 0.7.0-DEV.3216
@test Compat.AbstractDateTime === (isdefined(Compat.Dates, :AbstractDateTime) ? Compat.Dates.AbstractDateTime : Compat.Dates.TimeType)
@test Compat.AbstractDateTime <: Compat.Dates.TimeType
@test Compat.Dates.DateTime <: Compat.AbstractDateTime

# 0.7.0-DEV.2402

x = Compat.Dates.Second(172799)
@test floor(x, Compat.Dates.Week) == Compat.Dates.Week(0)
@test floor(x, Compat.Dates.Day) == Compat.Dates.Day(1)
@test floor(x, Compat.Dates.Hour) == Compat.Dates.Hour(47)
@test floor(x, Compat.Dates.Minute) == Compat.Dates.Minute(2879)
@test floor(x, Compat.Dates.Second) == Compat.Dates.Second(172799)
@test floor(x, Compat.Dates.Millisecond) == Compat.Dates.Millisecond(172799000)
@test ceil(x, Compat.Dates.Week) == Compat.Dates.Week(1)
@test ceil(x, Compat.Dates.Day) == Compat.Dates.Day(2)
@test ceil(x, Compat.Dates.Hour) == Compat.Dates.Hour(48)
@test ceil(x, Compat.Dates.Minute) == Compat.Dates.Minute(2880)
@test ceil(x, Compat.Dates.Second) == Compat.Dates.Second(172799)
@test ceil(x, Compat.Dates.Millisecond) == Compat.Dates.Millisecond(172799000)
@test round(x, Compat.Dates.Week) == Compat.Dates.Week(0)
@test round(x, Compat.Dates.Day) == Compat.Dates.Day(2)
@test round(x, Compat.Dates.Hour) == Compat.Dates.Hour(48)
@test round(x, Compat.Dates.Minute) == Compat.Dates.Minute(2880)
@test round(x, Compat.Dates.Second) == Compat.Dates.Second(172799)
@test round(x, Compat.Dates.Millisecond) == Compat.Dates.Millisecond(172799000)

x = Compat.Dates.Nanosecond(2000999999)
@test floor(x, Compat.Dates.Second) == Compat.Dates.Second(2)
@test floor(x, Compat.Dates.Millisecond) == Compat.Dates.Millisecond(2000)
@test floor(x, Compat.Dates.Microsecond) == Compat.Dates.Microsecond(2000999)
@test floor(x, Compat.Dates.Nanosecond) == x
@test ceil(x, Compat.Dates.Second) == Compat.Dates.Second(3)
@test ceil(x, Compat.Dates.Millisecond) == Compat.Dates.Millisecond(2001)
@test ceil(x, Compat.Dates.Microsecond) == Compat.Dates.Microsecond(2001000)
@test ceil(x, Compat.Dates.Nanosecond) == x
@test round(x, Compat.Dates.Second) == Compat.Dates.Second(2)
@test round(x, Compat.Dates.Millisecond) == Compat.Dates.Millisecond(2001)
@test round(x, Compat.Dates.Microsecond) == Compat.Dates.Microsecond(2001000)
@test round(x, Compat.Dates.Nanosecond) == x


for x in [Compat.Dates.Week(3), Compat.Dates.Day(14), Compat.Dates.Second(604800)]
    local x
    for p in [Compat.Dates.Week, Compat.Dates.Day, Compat.Dates.Hour, Compat.Dates.Second, Compat.Dates.Millisecond, Compat.Dates.Microsecond, Compat.Dates.Nanosecond]
        local p
        @test floor(x, p) == p(x)
        @test ceil(x, p) == p(x)
    end
end

x = Compat.Dates.Hour(36)
@test round(x, Compat.Dates.Day, RoundNearestTiesUp) == Compat.Dates.Day(2)
@test round(x, Compat.Dates.Day, RoundUp) == Compat.Dates.Day(2)
@test round(x, Compat.Dates.Day, RoundDown) == Compat.Dates.Day(1)
@test_throws DomainError round(x, Compat.Dates.Day, RoundNearest)
@test_throws DomainError round(x, Compat.Dates.Day, RoundNearestTiesAway)
@test_throws DomainError round(x, Compat.Dates.Day, RoundToZero)
@test round(x, Compat.Dates.Day) == round(x, Compat.Dates.Day, RoundNearestTiesUp)

x = Compat.Dates.Hour(86399)
for p in [Compat.Dates.Week, Compat.Dates.Day, Compat.Dates.Hour, Compat.Dates.Second, Compat.Dates.Millisecond, Compat.Dates.Microsecond, Compat.Dates.Nanosecond]
    local p
        for v in [-1, 0]
        @test_throws DomainError floor(x, p(v))
        @test_throws DomainError ceil(x, p(v))
        @test_throws DomainError round(x, p(v))
    end
end
for p in [Compat.Dates.Year, Compat.Dates.Month]
    local p
    for v in [-1, 0, 1]
        @test_throws MethodError floor(x, p(v))
        @test_throws MethodError ceil(x, p(v))
        @test_throws DomainError round(x, p(v))
    end
end

# 0.7.0-DEV.3025
let c = CartesianIndices((1:3, 1:2)), l = LinearIndices((1:3, 1:2))
    @test LinearIndices(c) == collect(l)
    @test CartesianIndices(l) == collect(c)
    @test first(c) == CartesianIndex(1, 1)
    @test CartesianIndex(1, 1) in c
    @test first(l) == 1
    @test size(c) == size(l) == (3, 2)
    @test c == collect(c) == [CartesianIndex(1, 1) CartesianIndex(1, 2)
                              CartesianIndex(2, 1) CartesianIndex(2, 2)
                              CartesianIndex(3, 1) CartesianIndex(3, 2)]
    @test l == collect(l) == reshape(1:6, 3, 2)
    @test c[1:6] == vec(c)
    @test l[1:6] == vec(l)
    # TODO the following test fails on current Julia master (since 0.7.0-DEV.4742), and
    # it's not clear yet whether it should work or not. See
    # https://github.com/JuliaLang/julia/pull/26682#issuecomment-379762632 and the
    # discussion following it
    #@test l == l[c] == map(i -> l[i], c)
    @test l[vec(c)] == collect(1:6)
    @test CartesianIndex(1, 1) in CartesianIndices((3, 4))
end

if !isdefined(Base, Symbol("@info"))
    let fname = tempname()
        try
            open(fname, "w") do fout
                redirect_stderr(fout) do
                    Compat.@info  "A"
                    Compat.@warn  "B"
                    oldstate = Compat.DEBUG[]
                    Compat.enable_debug(false)
                    Compat.@debug "C"
                    Compat.enable_debug(true)
                    Compat.@debug "D"
                    Compat.enable_debug(oldstate)
                    Compat.@error "E"
                end
            end
            @test read(fname, String) == (Base.have_color ? "\e[1m\e[36mInfo: \e[39m\e[22m\e[36mA\n\e[39m\e[1m\e[33mWarning: \e[39m\e[22m\e[33mB\e[39m\n\e[1m\e[34mDebug: \e[39m\e[22m\e[34mD\n\e[39m\e[1m\e[91mError: \e[39m\e[22m\e[91mE\n\e[39m" :
                "Info: A\nWarning: B\nDebug: D\nError: E\n")
        finally
            rm(fname, force=true)
        end
    end
end

# 0.7.0-DEV.3460
@test parentmodule(Compat.Sys) == Compat
@test parentmodule(sin) == Base
@test parentmodule(sin, Tuple{Int}) == Base.Math
@test parentmodule(Int) == Core
@test parentmodule(Array) == Core

@test codeunits("foo") == [0x66,0x6f,0x6f] == codeunits(SubString("fooαβγ",1,3))
@test ncodeunits("αβγ") == 6 == ncodeunits(SubString("fooαβγ",4,8))
@test codeunit("foo") == codeunit(SubString("fooαβγ",1,3)) == UInt8

# 0.7.0-DEV.3539
@test nameof(Compat.Sys) == :Sys
@test nameof(sin) == :sin
@test nameof(Float64) == :Float64
@test nameof(Array) == :Array

# 0.7.0-DEV.3382
module TestLibdl
    using Compat
    using Compat.Libdl
    using Compat.Test
    @test isdefined(@__MODULE__, :Libdl)
end

# 0.7.0-DEV.3516
@test argmax([10,12,9,11]) == 2
@test argmax([10 12; 9 11]) == CartesianIndex(1, 2)
@test argmax(Dict(:z=>10, :y=>12, :x=>9, :w=>11)) == :y
@test argmax((-5, 6, 10)) == 3
@test argmin([10,12,9,11]) == 3
@test argmin([10 12; 9 11]) == CartesianIndex(2, 1)
@test argmin(Dict(:z=>10, :y=>12, :x=>9, :w=>11)) == :x
@test argmin((1.0, -3, 0.f0)) == 2

# 0.7.0-DEV.3415
@test findall(x -> x==1, [1, 2, 3, 2, 1]) == [1, 5]

# 0.7.0-DEV.3500
module TestREPL
    using Compat
    using Compat.REPL
    using Compat.Test
    @test isdefined(@__MODULE__, :REPL)
end

# 0.7.0-DEV.3476
module TestSerialization
    using Compat
    using Compat.Serialization
    using Compat.Test
    @test isdefined(@__MODULE__, :Serialization)
    @test isdefined(@__MODULE__, :serialize)
    @test isdefined(@__MODULE__, :deserialize)
    if VERSION < v"1.0.0-DEV.44"
        @test isdefined(@__MODULE__, :SerializationState)
    end
end

module TestPkg
    using Compat
    using Compat.Pkg
    using Compat.Test
    @test isdefined(@__MODULE__, :Pkg)
    @test isdefined(Compat.Pkg, :add)
end

module TestInteractiveUtils
    using Compat
    using Compat.InteractiveUtils
    using Compat.Test
    @test isdefined(@__MODULE__, :InteractiveUtils)
    @test isdefined(@__MODULE__, :varinfo)
end

module TestLibGit2
    using Compat
    using Compat.LibGit2
    using Compat.Test
    @test isdefined(@__MODULE__, :LibGit2)
    @test isdefined(@__MODULE__, :GitRepo)
end

# 0.7.0-DEV.3469
@test GC.enable(true)
@test GC.enable(false)
@test !GC.enable(false)
@test !GC.enable(true)
@test GC.enable(true)

@test eltype(Base.Multimedia.displays) <: AbstractDisplay

# 0.7.0-DEV.3481
let b = IOBuffer()
    write(b, "hi")
    @test bytesavailable(b) == 0
end

# 0.7.0-DEV.3583
@test lastindex(zeros(4)) == 4
@test lastindex(zeros(4,4)) == 16
@test all(x -> firstindex(x) == 1, ([1, 2], [1 2; 3 4], 'a', 1, 1=>2, `foo`, "foo", (1, 2)))

# 0.7.0-DEV.3585
let buf = IOBuffer()
    if VERSION < v"0.7.0-DEV.3077"
        col = Base.have_color
        eval(Base, :(have_color = true))
        printstyled(buf, "foo", color=:red)
        eval(Base, :(have_color = $col))
    else
        printstyled(IOContext(buf, :color=>true), "foo", color=:red)
    end
    @test startswith(String(take!(buf)), Base.text_colors[:red])
end

# 0.7.0-DEV.3455
@test hasmethod(sin, Tuple{Float64})
let x = y = 1
    @test objectid(x) == objectid(y)
end

# 0.7.0-DEV.3415
for (f1, f2, i) in ((Compat.findfirst, Compat.findnext, 1),
                    (Compat.findlast, Compat.findprev, 2))
    # Generic methods
    @test f1(isequal(0), [1, 0]) == f2(isequal(0), [1, 0], i) == 2
    @test f1(isequal(9), [1, 0]) == f2(isequal(9), [1, 0], i) == nothing
    @test f1(in([0, 2]), [1, 0]) == f2(in([0, 2]), [1, 0], i) == 2
    @test f1(in([0, 2]), [1, 9]) == f2(in([0, 2]), [1, 9], i) == nothing
    if VERSION < v"0.7.0-DEV.4592"
        # test that occursin work on 0.6
        @test f1(occursin([0, 2]), [1, 0]) == f2(occursin([0, 2]), [1, 0], i) == 2
        @test f1(occursin([0, 2]), [1, 9]) == f2(occursin([0, 2]), [1, 9], i) == nothing
    end
    @test f1([true, false]) == f2([true, false], i) == 1
    @test f1([false, false]) == f2([false, false], i) == nothing

    # Specific methods
    @test f2(isequal('a'), "ba", i) == f1(isequal('a'), "ba") == 2
    for S in (Int8, UInt8), T in (Int8, UInt8)
        # Bug in Julia 0.6
        f1 === Compat.findlast && VERSION < v"0.7.0-DEV.3272" && continue
        @test f2(isequal(S(1)), T[0, 1], i) == f1(isequal(S(1)), T[0, 1]) == 2
        @test f2(isequal(S(9)), T[0, 1], i) == f1(isequal(S(9)), T[0, 1]) == nothing
    end
    for chars in (['a', 'z'], Set(['a', 'z']), ('a', 'z'))
        @test f2(in(chars), "ba", i) == f1(in(chars), "ba") == 2
        @test f2(in(chars), "bx", i) == f1(in(chars), "bx") == nothing
        if VERSION < v"0.7.0-DEV.4592"
            # test that occursin work on 0.6
            @test f2(occursin(chars), "ba", i) == f1(occursin(chars), "ba") == 2
            @test f2(occursin(chars), "bx", i) == f1(occursin(chars), "bx") == nothing
        end
    end
end
@test findnext("a", "ba", 1) == findfirst("a", "ba") == 2:2
@test findnext("z", "ba", 1) == findfirst("z", "ba") == (VERSION < v"0.7.0-DEV.4480" ? (0:-1) : nothing)
@test findprev("a", "ba", 2) == findlast("a", "ba") == 2:2
@test findprev("z", "ba", 2) == findlast("z", "ba") == (VERSION < v"0.7.0-DEV.4480" ? (0:-1) : nothing)
@test Compat.findnext("a", "ba", 1) == Compat.findfirst("a", "ba") == 2:2
@test Compat.findnext("z", "ba", 1) == Compat.findfirst("z", "ba") == nothing
@test Compat.findprev("a", "ba", 2) == Compat.findlast("a", "ba") == 2:2
@test Compat.findprev("z", "ba", 2) == Compat.findlast("z", "ba") == nothing

@test findnext(r"a", "ba", 1) == findfirst(r"a", "ba") == 2:2
@test findnext(r"z", "ba", 1) == findfirst(r"z", "ba") == (VERSION < v"0.7.0-DEV.4480" ? (0:-1) : nothing)
@test Compat.findnext(r"a", "ba", 1) == Compat.findfirst(r"a", "ba") == 2:2
@test Compat.findnext(r"z", "ba", 1) == Compat.findfirst(r"z", "ba") == nothing

@test findall([true, false, true]) == [1, 3]
@test findall(in([1, 2]), [1]) == [1]
if VERSION < v"0.7.0-DEV.4592"
    # test that occursin work on 0.6
    @test findall(occursin([1, 2]), [1]) == [1]
end

# 0.7.0-DEV.3666
module TestUUIDs
    using Compat
    using Compat.UUIDs
    using Compat.Test
    @test isdefined(@__MODULE__, :uuid1)
    @test isdefined(@__MODULE__, :uuid4)
    @test isdefined(@__MODULE__, :uuid_version)

    @test uuid_version(uuid1()) == 1
    @test uuid_version(uuid4()) == 4
    @test uuid1() isa UUID
    @test uuid4() isa UUID
end

# 0.7.0-DEV.3589
import Compat.Markdown
@test isa(Markdown.parse("foo"), Markdown.MD)

@test repr("text/plain", "string") == "\"string\"" #25990
@test showable("text/plain", 3.14159) #26089

# 25959
@test all(x -> isa(x, IO), (devnull, stdin, stdout, stderr))
@static if !isdefined(Base, :devnull)
    @test stdin === STDIN
    @test stdout === STDOUT
    @test stderr === STDERR
end

# 0.7.0-DEV.3666
module TestSockets
    using Compat
    using Compat.Sockets
    using Compat.Test

    @test isdefined(@__MODULE__, :UDPSocket)
    @test isdefined(@__MODULE__, :connect)
    @test isdefined(@__MODULE__, :listen)
    @test isdefined(@__MODULE__, :recv)

    @test ip"127.0.0.1".host == UInt32(2130706433)
end

# 0.7.0-DEV.3526
module TestNames
    export foo
    function bar end
end
@test :foo in Compat.names(TestNames)
@test :bar in Compat.names(TestNames, all=true)

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
# Issue #523
@test length(Compat.CartesianIndices((1:1,))) == 1
@test length(Compat.CartesianIndices((1:2,))) == 2
@test length(Compat.CartesianIndices((1:2, -1:1))) == 6

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

# 0.7.0-DEV.5171
let sep = Compat.Sys.iswindows() ? ';' : ':'
    withenv("PATH" => string(Compat.Sys.BINDIR, sep, get(ENV, "PATH", ""))) do
        jl = joinpath(Compat.Sys.BINDIR, "julia") * (Compat.Sys.iswindows() ? ".exe" : "")
        @test Compat.Sys.which("julia") == realpath(jl)
        @test Compat.Sys.isexecutable(jl)
        @test Compat.Sys.which("reallyseriouslynotathingyoushouldhave") === nothing
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

# 0.7.0-beta2.171
Random.seed!(1)
rng = MersenneTwister(0)
Random.seed!(rng, 1)
@test rand(rng) ≈ 0.23603334566204692
@test 0 < rand(Random.GLOBAL_RNG, Random.RangeGenerator(1:3)) < 4

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

nothing
