using Compat
using Dates
using Test
using UUIDs: UUID, uuid1, uuid_version

@test isempty(detect_ambiguities(Base, Core, Compat))

@testset "CartesianIndex" begin
    # https://github.com/JuliaLang/julia/pull/29440
    ci = CartesianIndex(1, 1)
    @test length(-ci:ci) == 9
    # https://github.com/JuliaLang/julia/pull/29442
    @test oneunit(ci) === ci
    # https://github.com/JuliaLang/julia/pull/30268
    A = randn(1,2,3)
    @test get(A, CartesianIndex(1,2,3), :some_default) === A[1,2,3]
    @test get(A, CartesianIndex(2,2,3), :some_default) === :some_default
    @test get(11:15, CartesianIndex(6), nothing) === nothing
    @test get(11:15, CartesianIndex(5), nothing) === 15
end

# julia#29679
@test !isnothing(1)
@test isnothing(nothing)

# https://github.com/JuliaLang/julia/pull/29749
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

# Support for positional `stop`
@test range(0, 5, length = 6) == 0.0:1.0:5.0
@test range(0, 10, step = 2) == 0:2:10
@test_throws ArgumentError range(0, 10)

mutable struct TLayout
    x::Int8
    y::Int16
    z::Int32
end
tlayout = TLayout(5,7,11)
@test hasfield(TLayout, :y)
@test !hasfield(TLayout, :a)
@test hasproperty(tlayout, :x)
@test !hasproperty(tlayout, :p)

@test merge((a=1,b=1)) == (a=1,b=1)
@test merge((a=1,), (b=2,), (c=3,)) == (a=1,b=2,c=3)

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
    @test @inferred(only(Ref([1, 2]))) == [1, 2]
    @test_throws ArgumentError only(Pair(10, 20))

    @test only(1 for ii in 1:1) === 1
    @test only(1 for ii in 1:10 if ii < 2) === 1
    @test_throws ArgumentError only(1 for ii in 1:10)
    @test_throws ArgumentError only(1 for ii in 1:10 if ii > 2)
    @test_throws ArgumentError only(1 for ii in 1:10 if ii > 200)
end

# https://github.com/JuliaLang/julia/pull/32628
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

# https://github.com/JuliaLang/julia/pull/31664
@testset "search character in strings" begin
    let astr = "Hello, world.\n",
        u8str = "∀ ε > 0, ∃ δ > 0: |x-y| < δ ⇒ |f(x)-f(y)| < ε"
        @test_throws BoundsError findnext('z', astr, 0)
        @test_throws BoundsError findnext('∀', astr, 0)
        @test findfirst('x', astr) == nothing
        @test findfirst('\0', astr) == nothing
        @test findfirst('\u80', astr) == nothing
        @test findfirst('∀', astr) == nothing
        @test findfirst('∀', u8str) == 1
        @test findfirst('ε', u8str) == 5
        @test findfirst('H', astr) == 1
        @test findfirst('l', astr) == 3
        @test findfirst('e', astr) == 2
        @test findfirst('u', astr) == nothing
        @test findnext('l', astr, 4) == 4
        @test findnext('l', astr, 5) == 11
        @test findnext('l', astr, 12) == nothing
        @test findfirst(',', astr) == 6
        @test findnext(',', astr, 7) == nothing
        @test findfirst('\n', astr) == 14
        @test findnext('\n', astr, 15) == nothing
        @test_throws BoundsError findnext('ε', astr, nextind(astr,lastindex(astr))+1)
        @test_throws BoundsError findnext('a', astr, nextind(astr,lastindex(astr))+1)
        @test findlast('x', astr) == nothing
        @test findlast('\0', astr) == nothing
        @test findlast('\u80', astr) == nothing
        @test findlast('∀', astr) == nothing
        @test findlast('∀', u8str) == 1
        @test findlast('ε', u8str) == 54
        @test findlast('H', astr) == 1
        @test findprev('H', astr, 0) == nothing
        @test findlast('l', astr) == 11
        @test findprev('l', astr, 5) == 4
        @test findprev('l', astr, 4) == 4
        @test findprev('l', astr, 3) == 3
        @test findprev('l', astr, 2) == nothing
        @test findlast(',', astr) == 6
        @test findprev(',', astr, 5) == nothing
        @test findlast('\n', astr) == 14
    end
end

using LinearAlgebra, Random

@testset "generalized dot #32739" begin
    Random.seed!(42) # https://github.com/JuliaLang/Compat.jl/issues/712

    # stdlib/LinearAlgebra/test/generic.jl
    for elty in (Int, Float32, Float64, BigFloat, Complex{Float32}, Complex{Float64}, Complex{BigFloat})
        n = 10
        if elty <: Int
            A = rand(-n:n, n, n)
            x = rand(-n:n, n)
            y = rand(-n:n, n)
        elseif elty <: Real
            A = convert(Matrix{elty}, randn(n,n))
            x = rand(elty, n)
            y = rand(elty, n)
        else
            A = convert(Matrix{elty}, complex.(randn(n,n), randn(n,n)))
            x = rand(elty, n)
            y = rand(elty, n)
        end
        @test dot(x, A, y) ≈ dot(A'x, y) ≈ *(x', A, y) ≈ (x'A)*y
        @test dot(x, A', y) ≈ dot(A*x, y) ≈ *(x', A', y) ≈ (x'A')*y
        elty <: Real && @test dot(x, transpose(A), y) ≈ dot(x, transpose(A)*y) ≈ *(x', transpose(A), y) ≈ (x'*transpose(A))*y
        B = reshape([A], 1, 1)
        x = [x]
        y = [y]
        @test dot(x, B, y) ≈ dot(B'x, y)
        @test dot(x, B', y) ≈ dot(B*x, y)
        elty <: Real && @test dot(x, transpose(B), y) ≈ dot(x, transpose(B)*y)
    end

    # stdlib/LinearAlgebra/test/symmetric.jl
    n = 10
    areal = randn(n,n)/2
    aimg  = randn(n,n)/2
    @testset for eltya in (Float32, Float64, ComplexF32, ComplexF64, BigFloat, Int)
        a = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(areal, aimg) : areal)
        asym = transpose(a) + a                 # symmetric indefinite
        aherm = a' + a                 # Hermitian indefinite
        apos  = a' * a                 # Hermitian positive definite
        aposs = apos + transpose(apos)        # Symmetric positive definite
        ε = εa = eps(abs(float(one(eltya))))
        x = randn(n)
        y = randn(n)
        b = randn(n,n)/2
        x = eltya == Int ? rand(1:7, n) : convert(Vector{eltya}, eltya <: Complex ? complex.(x, zeros(n)) : x)
        y = eltya == Int ? rand(1:7, n) : convert(Vector{eltya}, eltya <: Complex ? complex.(y, zeros(n)) : y)
        b = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(b, zeros(n,n)) : b)

        @testset "generalized dot product" begin
            for uplo in (:U, :L)
                @test dot(x, Hermitian(aherm, uplo), y) ≈ dot(x, Hermitian(aherm, uplo)*y) ≈ dot(x, Matrix(Hermitian(aherm, uplo)), y)
                @test dot(x, Hermitian(aherm, uplo), x) ≈ dot(x, Hermitian(aherm, uplo)*x) ≈ dot(x, Matrix(Hermitian(aherm, uplo)), x)
            end
            if eltya <: Real
                for uplo in (:U, :L)
                    @test dot(x, Symmetric(aherm, uplo), y) ≈ dot(x, Symmetric(aherm, uplo)*y) ≈ dot(x, Matrix(Symmetric(aherm, uplo)), y)
                    @test dot(x, Symmetric(aherm, uplo), x) ≈ dot(x, Symmetric(aherm, uplo)*x) ≈ dot(x, Matrix(Symmetric(aherm, uplo)), x)
                end
            end
        end
    end

    # stdlib/LinearAlgebra/test/uniformscaling.jl
    @testset "generalized dot" begin
        x = rand(-10:10, 3)
        y = rand(-10:10, 3)
        λ = rand(-10:10)
        J = UniformScaling(λ)
        @test dot(x, J, y) == λ*dot(x, y)
    end

    # stdlib/LinearAlgebra/test/bidiag.jl
    # The special method for this is not in Compat #683, so this tests the generic fallback
    @testset "generalized dot" begin
        for elty in (Float64, ComplexF64)
            dv = randn(elty, 5)
            ev = randn(elty, 4)
            x = randn(elty, 5)
            y = randn(elty, 5)
            for uplo in (:U, :L)
                B = Bidiagonal(dv, ev, uplo)
                @test dot(x, B, y) ≈ dot(B'x, y) ≈ dot(x, Matrix(B), y)
            end
        end
    end

    # Diagonal -- no such test in Base.
    @testset "diagonal" begin
        x = rand(-10:10, 3) .+ im
        y = rand(-10:10, 3) .+ im
        d = Diagonal(rand(-10:10, 3) .+ im)
        @test dot(x,d,y) == dot(x,collect(d),y) == dot(x, d*y)
    end
end

# https://github.com/JuliaLang/julia/pull/33568
@testset "function composition" begin
    @test ∘(x -> x-2, x -> x-3, x -> x+5)(7) == 7
    fs = [x -> x[1:2], uppercase, lowercase]
    @test ∘(fs...)("ABC") == "AB"

    # https://github.com/JuliaLang/julia/pull/34251
    @testset "unary" begin
        @test ∘(identity) === identity
        @test ∘(inv) === inv
    end
end

# https://github.com/JuliaLang/julia/pull/33128
@testset "pkgdir" begin
    @test pkgdir(Main) === nothing
    @test joinpath(pkgdir(Compat), "") == abspath(joinpath(@__DIR__, ".."))
end

# https://github.com/JuliaLang/julia/pull/33736/
@testset "ReverseOrdering constructor" begin
    @test Base.Order.ReverseOrdering() == Base.Order.Reverse
end

# https://github.com/JuliaLang/julia/pull/32968
@testset "filter on Tuples" begin
    @test filter(isodd, (1,2,3)) == (1, 3)
    @test filter(isequal(2), (true, 2.0, 3)) === (2.0,)
    @test filter(i -> true, ()) == ()
    @test filter(identity, (true,)) === (true,)
    longtuple = ntuple(identity, 20)
    @test filter(iseven, longtuple) == ntuple(i->2i, 10)
    @test filter(x -> x<2, (longtuple..., 1.5)) === (1, 1.5)
end

# https://github.com/JuliaLang/julia/pull/34652
@testset "ismutable" begin
    @test ismutable(1) == false
    @test ismutable([]) == true
end

# https://github.com/JuliaLang/julia/pull/28761
@testset "uuid5" begin
    u1 = uuid1()
    u5 = uuid5(u1, "julia")
    @test uuid_version(u5) == 5
    @test u5 == UUID(string(u5)) == UUID(GenericString(string(u5)))
    @test u5 == UUID(UInt128(u5))

    following_uuids = [
        UUID("22b4a8a1-e548-4eeb-9270-60426d66a48e"),
        UUID("30ea6cfd-c270-569f-b4cb-795dead63686"),
        UUID("31099374-e3a0-5fde-9482-791c639bf29b"),
        UUID("6b34b357-a348-53aa-8c71-fb9b06c3a51e"),
        UUID("fdbd7d4d-c462-59cc-ae6a-0c3b010240e2"),
        UUID("d8cc6298-75d5-57e0-996c-279259ab365c"),
    ]

    for (idx, init_uuid) in enumerate(following_uuids[1:end-1])
        next_id = uuid5(init_uuid, "julia")
        @test next_id == following_uuids[idx+1]
    end

    # Some UUID namespaces provided in the appendix of RFC 4122
    # https://tools.ietf.org/html/rfc4122.html#appendix-C
    namespace_dns  = UUID(0x6ba7b8109dad11d180b400c04fd430c8) # 6ba7b810-9dad-11d1-80b4-00c04fd430c8
    namespace_url  = UUID(0x6ba7b8119dad11d180b400c04fd430c8) # 6ba7b811-9dad-11d1-80b4-00c04fd430c8
    namespace_oid  = UUID(0x6ba7b8129dad11d180b400c04fd430c8) # 6ba7b812-9dad-11d1-80b4-00c04fd430c8
    namespace_x500 = UUID(0x6ba7b8149dad11d180b400c04fd430c8) # 6ba7b814-9dad-11d1-80b4-00c04fd430c8

    # Python-generated UUID following each of the standard namespaces
    standard_namespace_uuids = [
        (namespace_dns,  UUID("00ca23ad-40ef-500c-a910-157de3950d07")),
        (namespace_oid,  UUID("b7bf72b0-fb4e-538b-952a-3be296f07f6d")),
        (namespace_url,  UUID("997cd5be-4705-5439-9fe6-d77b18d612e5")),
        (namespace_x500, UUID("993c6684-82e7-5cdb-bd46-9bff0362e6a9")),
    ]

    for (init_uuid, next_uuid) in standard_namespace_uuids
        result = uuid5(init_uuid, "julia")
        @test next_uuid == result
    end
end

@testset "Irrational zero and one" begin
    @test one(pi) === true
    @test zero(pi) === false
    @test one(typeof(pi)) === true
    @test zero(typeof(pi)) === false
end

# https://github.com/JuliaLang/julia/pull/32753
@testset "evalpoly real" begin
    for x in -1.0:2.0, p1 in -3.0:3.0, p2 in -3.0:3.0, p3 in -3.0:3.0
        evpm = @evalpoly(x, p1, p2, p3)
        @test evalpoly(x, (p1, p2, p3)) == evpm
        @test evalpoly(x, [p1, p2, p3]) == evpm
    end
end
@testset "evalpoly complex" begin
    for x in -1.0:2.0, y in -1.0:2.0, p1 in -3.0:3.0, p2 in -3.0:3.0, p3 in -3.0:3.0
        z = x + im * y
        evpm = @evalpoly(z, p1, p2, p3)
        @test evalpoly(z, (p1, p2, p3)) == evpm
        @test evalpoly(z, [p1, p2, p3]) == evpm
    end
    @test evalpoly(1+im, (2,)) == 2
    @test evalpoly(1+im, [2,]) == 2
end

# https://github.com/JuliaLang/julia/pull/35298
begin
    # A custom linear slow sparse-like array that relies upon Dict for its storage
    struct TSlow{T,N} <: AbstractArray{T,N}
        data::Dict{NTuple{N,Int}, T}
        dims::NTuple{N,Int}
    end
    TSlow(::Type{T}, dims::Int...) where {T} = TSlow(T, dims)
    TSlow(::Type{T}, dims::NTuple{N,Int}) where {T,N} = TSlow{T,N}(Dict{NTuple{N,Int}, T}(), dims)

    TSlow{T,N}(X::TSlow{T,N})         where {T,N  } = X
    TSlow(     X::AbstractArray{T,N}) where {T,N  } = TSlow{T,N}(X)
    TSlow{T  }(X::AbstractArray{_,N}) where {T,N,_} = TSlow{T,N}(X)
    TSlow{T,N}(X::AbstractArray     ) where {T,N  } = begin
        A = TSlow(T, size(X))
        for I in CartesianIndices(X)
            A[Tuple(I)...] = X[Tuple(I)...]
        end
        A
    end
    Base.size(A::TSlow) = A.dims
    Base.similar(A::TSlow, ::Type{T}, dims::Dims) where {T} = TSlow(T, dims)
    Base.IndexStyle(::Type{A}) where {A<:TSlow} = IndexCartesian()
    Base.getindex(A::TSlow{T,N}, i::Vararg{Int,N}) where {T,N} = get(A.data, i, zero(T))
    Base.setindex!(A::TSlow{T,N}, v, i::Vararg{Int,N}) where {T,N} = (A.data[i] = v)
end

# https://github.com/JuliaLang/julia/pull/35304
@testset "similar(PermutedDimsArray)" begin
    x = PermutedDimsArray([1 2; 3 4], (2, 1))
    @test similar(x, 3,3) isa Array
    z = TSlow([1 2; 3 4])
    x_slow = PermutedDimsArray(z, (2, 1))
    @test similar(x_slow, 3,3) isa TSlow
end

# https://github.com/JuliaLang/julia/pull/34548
@testset "@NamedTuple" begin
    @test (@NamedTuple {a::Int, b::String}) === NamedTuple{(:a, :b),Tuple{Int,String}} ===
        @NamedTuple begin
            a::Int
            b::String
        end
    @test (@NamedTuple {a::Int, b}) === NamedTuple{(:a, :b),Tuple{Int,Any}}
end

struct NonFunctionCallable end
(::NonFunctionCallable)(args...) = +(args...)

@testset "mergewith" begin
    d1 = Dict("A" => 1, "B" => 2)
    d2 = Dict("B" => 3.0, "C" => 4.0)
    @test mergewith(+, d1, d2) == Dict("A" => 1, "B" => 5, "C" => 4)
    @test mergewith(*, d1, d2) == Dict("A" => 1, "B" => 6, "C" => 4)
    @test mergewith(-, d1, d2) == Dict("A" => 1, "B" => -1, "C" => 4)
    @test mergewith(NonFunctionCallable(), d1, d2) == Dict("A" => 1, "B" => 5, "C" => 4)
    @test foldl(mergewith(+), [d1, d2]; init=Dict{Union{},Union{}}()) ==
        Dict("A" => 1, "B" => 5, "C" => 4)
end

@testset "mergewith!" begin
    d1 = Dict("A" => 1, "B" => 3, "C" => 4)
    d2 = Dict("B" => 3, "C" => 4)
    mergewith!(+, d1, d2)
    @test d1 == Dict("A" => 1, "B" => 6, "C" => 8)
    mergewith!(*, d1, d2)
    @test d1 == Dict("A" => 1, "B" => 18, "C" => 32)
    mergewith!(-, d1, d2)
    @test d1 == Dict("A" => 1, "B" => 15, "C" => 28)
    mergewith!(NonFunctionCallable(), d1, d2)
    @test d1 == Dict("A" => 1, "B" => 18, "C" => 32)
    @test foldl(mergewith!(+), [d1, d2]; init=empty(d1)) ==
        Dict("A" => 1, "B" => 21, "C" => 36)
end

# https://github.com/JuliaLang/julia/pull/34427
@testset "isdisjoint" begin
    for S in (Set, BitSet, Vector)
        for (l,r) in ((S([1,2]),     S([3,4])),
                      (S([5,6,7,8]), S([7,8,9])),
                      (S([1,2]),     S([3,4])),
                      (S([5,6,7,8]), S([7,8,9])),
                      (S([1,2,3]),   S()),
                      (S(),          S()),
                      (S(),          S([1,2,3])),
                      (S([1,2,3]),   S([1])),
                      (S([1,2,3]),   S([1,2])),
                      (S([1,2,3]),   S([1,2,3])),
                      (S([1,2,3]),   S([4])),
                      (S([1,2,3]),   S([4,1])))
            @test isdisjoint(l,l) == isempty(l)
            @test isdisjoint(l,r) == isempty(intersect(l,r))
        end
    end
end

# https://github.com/JuliaLang/julia/pull/35577
@testset "union on OneTo" begin
    @test union(Base.OneTo(3), Base.OneTo(4)) === Base.OneTo(4)
end

# https://github.com/JuliaLang/julia/pull/35929
# https://github.com/JuliaLang/julia/pull/29135
@testset "strided transposes" begin
    for t in (Adjoint, Transpose)
        @test strides(t(rand(3))) == (3, 1)
        @test strides(t(rand(3,2))) == (3, 1)
        @test strides(t(view(rand(3, 2), :))) == (6, 1)
        @test strides(t(view(rand(3, 2), :, 1:2))) == (3, 1)

         A = rand(3)
        @test pointer(t(A)) === pointer(A)
        B = rand(3,1)
        @test pointer(t(B)) === pointer(B)
    end
    @test_throws MethodError strides(Adjoint(rand(3) .+ rand(3).*im))
    @test_throws MethodError strides(Adjoint(rand(3, 2) .+ rand(3, 2).*im))
    @test strides(Transpose(rand(3) .+ rand(3).*im)) == (3, 1)
    @test strides(Transpose(rand(3, 2) .+ rand(3, 2).*im)) == (3, 1)

     C = rand(3) .+ rand(3).*im
    @test_throws ErrorException pointer(Adjoint(C))
    @test pointer(Transpose(C)) === pointer(C)
    D = rand(3,2) .+ rand(3,2).*im
    @test_throws ErrorException pointer(Adjoint(D))
    @test pointer(Transpose(D)) === pointer(D)
end

# https://github.com/JuliaLang/julia/pull/27516
@testset "two arg @inferred" begin
    g(a) = a < 10 ? missing : 1
    @test ismissing(g(9))
    @test g(10) == 1
    @inferred Missing g(9)
    @inferred Missing g(10)
end

# https://github.com/JuliaLang/julia/pull/36360
@testset "get_set_num_threads" begin
    default = Compat.get_num_threads()
    @test default isa Int # seems dodgy, could be nothing!
    @test default > 0
    Compat.set_num_threads(1)
    @test Compat.get_num_threads() === 1
    Compat.set_num_threads(default)
    @test Compat.get_num_threads() === default

    # Run the ::Nothing method, to check no error:
    Compat.set_num_threads(nothing)
    Compat.set_num_threads(default)

    if VERSION < v"1.6.0-DEV.322"
        # These tests from PR rely on internal functions which would be BLAS. not Compat.
        @test_logs (:warn,) match_mode=:any Compat._set_num_threads(1, _blas=:unknown)
        if Compat.guess_vendor() !== :osxblas
            # test osxblas which is not covered by CI
            withenv("VECLIB_MAXIMUM_THREADS" => nothing) do
                @test @test_logs(
                    (:warn,),
                    (:warn,),
                    match_mode=:any,
                    Compat._get_num_threads(_blas=:osxblas),
                ) === nothing
                @test_logs Compat._set_num_threads(1, _blas=:osxblas)
                @test @test_logs(Compat._get_num_threads(_blas=:osxblas)) === 1
                @test_logs Compat._set_num_threads(2, _blas=:osxblas)
                @test @test_logs(Compat._get_num_threads(_blas=:osxblas)) === 2
            end
        end
    end
end

# https://github.com/JuliaLang/julia/pull/30915
@testset "curried comparisons" begin
    eql5 = (==)(5)
    neq5 = (!=)(5)
    gte5 = (>=)(5)
    lte5 = (<=)(5)
    gt5  = (>)(5)
    lt5  = (<)(5)

    @test eql5(5) && !eql5(0)
    @test neq5(6) && !neq5(5)
    @test gte5(5) && gte5(6)
    @test lte5(5) && lte5(4)
    @test gt5(6) && !gt5(5)
    @test lt5(4) && !lt5(5)
end

@testset "contains" begin
    @test contains("foo", "o")
    @test contains("o")("foo")
end

# https://github.com/JuliaLang/julia/pull/35052
@testset "curried startswith/endswith" begin
    @test startswith("a")("abcd")
    @test endswith("d")("abcd")
end

# https://github.com/JuliaLang/julia/pull/37517
@testset "ComposedFunction" begin
    @test sin ∘ cos isa Compat.ComposedFunction
    @test sin ∘ cos === Compat.ComposedFunction(sin, cos)
    c = sin ∘ cos
    @test c.outer === sin
    @test c.inner === cos
    if VERSION < v"1.6.0-DEV.1037"
        @test c.f === sin
        @test c.g === cos
        @test propertynames(c) == (:f, :g, :outer, :inner)
    else
        @test propertynames(c) == (:outer, :inner)
    end
end

# From spawn.jl
shcmd = `sh`
havebb = false
if Sys.iswindows()
    busybox = download("https://cache.julialang.org/https://frippery.org/files/busybox/busybox.exe", joinpath(tempdir(), "busybox.exe"))
    havebb = try # use busybox-w32 on windows, if available
        success(`$busybox`)
        true
    catch
        false
    end
    if havebb
        shcmd = `$busybox sh`
    end
end

# https://github.com/JuliaLang/julia/pull/37244
@testset "addenv()" begin
    cmd = Cmd(`$shcmd -c "echo \$FOO \$BAR"`, env=Dict("FOO" => "foo"))
    @test strip(String(read(cmd))) == "foo"
    cmd = addenv(cmd, "BAR" => "bar")
    @test strip(String(read(cmd))) == "foo bar"
    cmd = addenv(cmd, Dict("FOO" => "bar"))
    @test strip(String(read(cmd))) == "bar bar"
    cmd = addenv(cmd, ["FOO=baz"])
    @test strip(String(read(cmd))) == "baz bar"
end

# https://github.com/JuliaLang/julia/pull/37559
@testset "reinterpred(reshape, ...)" begin
    # simplified from PR
    Ar = Int64[1 3; 2 4]
    @test @inferred(ndims(reinterpret(reshape, Complex{Int64}, Ar))) == 1
    @test @inferred(axes(reinterpret(reshape, Complex{Int64}, Ar))) === (Base.OneTo(2),)
    @test @inferred(size(reinterpret(reshape, Complex{Int64}, Ar))) == (2,)

    _B = Complex{Int64}[5+6im, 7+8im, 9+10im]
    @test @inferred(ndims(reinterpret(reshape, Int64, _B))) == 2
    @test @inferred(axes(reinterpret(reshape, Int64, _B))) === (Base.OneTo(2), Base.OneTo(3))
    @test @inferred(size(reinterpret(reshape, Int64, _B))) == (2, 3)
    @test @inferred(ndims(reinterpret(reshape, Int128, _B))) == 1
    @test @inferred(axes(reinterpret(reshape, Int128, _B))) === (Base.OneTo(3),)
    @test @inferred(size(reinterpret(reshape, Int128, _B))) == (3,)

    A = Int64[1, 2, 3, 4]
    Av = [Int32[1,2], Int32[3,4]]

    @test_throws ArgumentError reinterpret(Vector{Int64}, A) # ("cannot reinterpret `Int64` as `Vector{Int64}`, type `Vector{Int64}` is not a bits type")
    @test_throws ArgumentError reinterpret(Int32, Av) # ("cannot reinterpret `Vector{Int32}` as `Int32`, type `Vector{Int32}` is not a bits type")
    @test_throws ArgumentError("cannot reinterpret a zero-dimensional `Int64` array to `Int32` which is of a different size") reinterpret(Int32, reshape([Int64(0)]))
    @test_throws ArgumentError("cannot reinterpret a zero-dimensional `Int32` array to `Int64` which is of a different size") reinterpret(Int64, reshape([Int32(0)]))
    @test_throws ArgumentError reinterpret(Tuple{Int,Int}, [1,2,3,4,5]) # ("""cannot reinterpret an `$Int` array to `Tuple{$Int, $Int}` whose first dimension has size `5`.
                               # The resulting array would have non-integral first dimension.
                               # """)
    @test_throws ArgumentError("`reinterpret(reshape, Complex{Int64}, a)` where `eltype(a)` is Int64 requires that `axes(a, 1)` (got Base.OneTo(4)) be equal to 1:2 (from the ratio of element sizes)") reinterpret(reshape, Complex{Int64}, A)
    @test_throws ArgumentError("`reinterpret(reshape, T, a)` requires that one of `sizeof(T)` (got 24) and `sizeof(eltype(a))` (got 16) be an integer multiple of the other") reinterpret(reshape, NTuple{3, Int64}, _B)
    @test_throws ArgumentError reinterpret(reshape, Vector{Int64}, Ar) # ("cannot reinterpret `Int64` as `Vector{Int64}`, type `Vector{Int64}` is not a bits type")
    @test_throws ArgumentError("cannot reinterpret a zero-dimensional `UInt8` array to `UInt16` which is of a larger size") reinterpret(reshape, UInt16, reshape([0x01]))

    # getindex
    _A = A
    @test reinterpret(Complex{Int64}, _A) == [1 + 2im, 3 + 4im]
    @test reinterpret(Float64, _A) == reinterpret.(Float64, A)
    @test reinterpret(reshape, Float64, _A) == reinterpret.(Float64, A)

    Ars = Ar
    @test reinterpret(reshape, Complex{Int64}, Ar) == [1 + 2im, 3 + 4im]
    @test reinterpret(reshape, Float64, Ar) == reinterpret.(Float64, Ars)

    # setindex
    A3 = collect(reshape(1:18, 2, 3, 3))
    A3r = reinterpret(reshape, Complex{Int}, A3)
    @test A3r[4] === A3r[1,2] === A3r[CartesianIndex(1, 2)] === 7+8im
    A3r[2,3] = -8-15im
    @test A3[1,2,3] == -8
    @test A3[2,2,3] == -15
    A3r[4] = 100+200im
    @test A3[1,1,2] == 100
    @test A3[2,1,2] == 200
    A3r[CartesianIndex(1,2)] = 300+400im
    @test A3[1,1,2] == 300
    @test A3[2,1,2] == 400

    # Test 0-dimensional Arrays
    A = zeros(UInt32)
    B = reinterpret(Int32,A)
    Brs = reinterpret(reshape,Int32,A)
    @test size(B) == size(Brs) == ()
    @test axes(B) == axes(Brs) == ()
    B[] = Int32(5)
    @test B[] === Int32(5)
    @test Brs[] === Int32(5)
    @test A[] === UInt32(5)

    # reductions
    a = [(1,2,3), (4,5,6)]
    ars = reinterpret(reshape, Int, a)
    @test sum(ars) == 21
    @test sum(ars; dims=1) == [6 15]
    @test sum(ars; dims=2) == reshape([5,7,9], (3, 1))
    @test sum(ars; dims=(1,2)) == reshape([21], (1, 1))
    # also test large sizes for the pairwise algorithm
    a = [(k,k+1,k+2) for k = 1:3:4000]
    ars = reinterpret(reshape, Int, a)
    @test sum(ars) == 8010003
end

# https://github.com/JuliaLang/julia/pull/29634
@testset "5-arg mul!" begin
    A = [1.0 2.0; 3.0 4.0]
    B = [10.0, 2.0]
    C = [100.0, 1000.0]
    x = -20.0
    alpha = 0.1
    beta = 0.1

    Cmut = copy(C)
    @test  Cmut == mul!(Cmut, A, B, alpha, beta) ≈ ((A * B * alpha) + (C * beta))

    Cmut = copy(C)
    @test  Cmut == mul!(Cmut, x, B, alpha, beta) ≈ ((x * B * alpha) + (C * beta))

    Cmut = copy(C)
    @test  Cmut == mul!(Cmut, B, x, alpha, beta) ≈ ((B * x * alpha) + (C * beta))
end

# https://github.com/JuliaLang/julia/pull/35243
@testset "parseatom and parseall" begin
    @test Compat.parseatom(raw"foo$(@bar)baz", 5; filename="foo") ==
        (Expr(:macrocall, Symbol("@bar"), LineNumberNode(1, :foo)), 11)

    ex = Compat.parseall(
        raw"""
        begin
            @a b
            @c() + 1
        end
        """;
        filename="foo",
    )
    @test ex == Expr(:toplevel,
        LineNumberNode(1, :foo),
        Expr(:block,
            LineNumberNode(2, :foo),
            Expr(:macrocall, Symbol("@a"), LineNumberNode(2, :foo), :b),
            LineNumberNode(3, :foo),
            Expr(:call,
                 :+,
                 Expr(:macrocall, Symbol("@c"), LineNumberNode(3, :foo)),
                 1,
            ),
        ),
    )

    ex = Compat.parseall(
        raw"""

        begin a = 1 end

        begin
            b = 2
        end
        """;
        filename="foo",
    )
    @test ex == Expr(:toplevel,
        LineNumberNode(2, :foo),
        Expr(:block,
            LineNumberNode(2, :foo),
            :(a = 1),
        ),
        LineNumberNode(4, :foo),
        Expr(:block,
            LineNumberNode(5, :foo),
            :(b = 2),
        ),
    )
end

# https://github.com/JuliaLang/julia/pull/37391
@testset "Dates.canonicalize(::Period)" begin
    # reduce individual Period into most basic CompoundPeriod
    @test Dates.canonicalize(Dates.Nanosecond(1000000)) == Dates.canonicalize(Dates.Millisecond(1))
    @test Dates.canonicalize(Dates.Millisecond(1000)) == Dates.canonicalize(Dates.Second(1))
    @test Dates.canonicalize(Dates.Second(60)) == Dates.canonicalize(Dates.Minute(1))
    @test Dates.canonicalize(Dates.Minute(60)) == Dates.canonicalize(Dates.Hour(1))
    @test Dates.canonicalize(Dates.Hour(24)) == Dates.canonicalize(Dates.Day(1))
    @test Dates.canonicalize(Dates.Day(7)) == Dates.canonicalize(Dates.Week(1))
    @test Dates.canonicalize(Dates.Month(12)) == Dates.canonicalize(Dates.Year(1))
    @test Dates.canonicalize(Dates.Minute(24*60*1 + 12*60)) == Dates.canonicalize(Dates.CompoundPeriod([Dates.Day(1),Dates.Hour(12)]))
end

# https://github.com/JuliaLang/julia/pull/35816
@testset "sincospi(x)" begin
    @test sincospi(0.13) == (sinpi(0.13), cospi(0.13))
    @test sincospi(1//3) == (sinpi(1//3), cospi(1//3))
    @test sincospi(5) == (sinpi(5), cospi(5))
    @test sincospi(ℯ) == (sinpi(ℯ), cospi(ℯ))
    @test sincospi(0.13im) == (sinpi(0.13im), cospi(0.13im))
end

# https://github.com/JuliaLang/julia/pull/38449
@testset "cispi(x)" begin
    @test cispi(true) == -1 + 0im
    @test cispi(1)    == -1.0 + 0.0im
    @test cispi(2.0)  == 1.0 + 0.0im
    @test cispi(0.25 + 1im) ≈ cis(π/4 + π*im)
end

include("iterators.jl")

# Import renaming, https://github.com/JuliaLang/julia/pull/37396,
# and https://github.com/JuliaLang/julia/pull/37965
module ImportRename
    using Compat
    @compat import LinearAlgebra as LA
    @compat import LinearAlgebra.BLAS as BL
    @compat import LinearAlgebra.BLAS: dotc as dc
    @compat import LinearAlgebra: cholesky as chol, lu as lufact
    @compat using LinearAlgebra.BLAS: hemm as hm
end

import .ImportRename
import LinearAlgebra

@testset "import renaming" begin
    @test ImportRename.LA === LinearAlgebra
    @test !isdefined(ImportRename, :LinearAlgebra)
    @test ImportRename.BL === LinearAlgebra.BLAS
    @test !isdefined(ImportRename, :BLAS)
    @test ImportRename.dc === LinearAlgebra.BLAS.dotc
    @test !isdefined(ImportRename, :dotc)
    @test ImportRename.chol === LinearAlgebra.cholesky
    @test ImportRename.lufact === LinearAlgebra.lu
    @test ImportRename.hm === LinearAlgebra.BLAS.hemm
    @test !isdefined(ImportRename, :hemm)
end

# https://github.com/JuliaLang/julia/pull/29790
@testset "regex startswith and endswith" begin
    @test startswith("abc", r"a")
    @test startswith("abc", r"ab")
    @test endswith("abc", r"c")
    @test endswith("abc", r"bc")
    @test !startswith("abc", r"b")
    @test !startswith("abc", r"c")
    @test !startswith("abc", r"bc")
    @test !endswith("abc", r"a")
    @test !endswith("abc", r"b")
    @test !endswith("abc", r"ab")

    @test !startswith("abc", r"A")
    @test !startswith("abc", r"aB")
    @test startswith("abc", r"A"i)
    @test startswith("abc", r"aB"i)
    @test !endswith("abc", r"C")
    @test !endswith("abc", r"Bc")
    @test endswith("abc", r"C"i)
    @test endswith("abc", r"Bc"i)
end
