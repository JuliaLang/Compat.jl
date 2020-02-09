using Compat
using Test
using UUIDs: UUID, uuid1, uuid_version

@test isempty(detect_ambiguities(Base, Core, Compat))

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

using LinearAlgebra

@testset "generalized dot #32739" begin
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

nothing
