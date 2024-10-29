using Compat
using Test

@test isempty(detect_ambiguities(Base, Core, Compat))

module Mod50105
    using Compat
    @compat public foo, var"#", baz
    @compat public @mac1
    @compat public f00, @mac2, @mac3
    @compat public @mac4, @mac5
end

# https://github.com/JuliaLang/julia/pull/50105
@testset "@compat public" begin
    @compat public foo_50105
    # foo_50105 = 4 # Uncommenting this line would cause errors due to https://github.com/JuliaLang/julia/issues/51325
    @test Base.isexported(@__MODULE__, :foo_50105) === false
    VERSION >= v"1.11.0-DEV.469" && @test Base.ispublic(@__MODULE__, :foo_50105)
    for sym in [:foo, Symbol("#"), :baz, Symbol("@mac1"), :f00, Symbol("@mac2"), Symbol("@mac3"), Symbol("@mac4"), Symbol("@mac5")]
        @test Base.isexported(Mod50105, sym) === false
        VERSION >= v"1.11.0-DEV.469" && @test Base.ispublic(Mod50105, sym)
    end

    @test_throws LoadError @eval @compat public 4, bar
    @test_throws LoadError @eval @compat public foo bar
    @test_throws LoadError @eval @compat publac foo, bar
    @test_throws LoadError @eval @compat public 4, @bar
    @test_throws LoadError @eval @compat public foo @bar
    @test_throws LoadError @eval @compat publac foo, @bar
    @test_throws LoadError @eval @compat public @bar, 4
    @test_throws LoadError @eval @compat public @bar foo
    @test_throws LoadError @eval @compat publac @bar, foo
end

# https://github.com/JuliaLang/julia/pull/47679
@testset "allunique(f, xs)" begin
    @test allunique(sin, 1:3)
    @test !allunique(sin, [1,2,3,1])
    @test allunique(sin, (1, 2, pi, im))  # eltype Any
    @test allunique(abs2, 1:100)
    @test !allunique(abs, -10:10)
    @test allunique(abs2, Vector{Any}(1:100))
    # These cases don't call the function at all:
    @test allunique(error, [])
    @test_skip allunique(error, [1]) # depends on updated code in Base to work
end
@testset "allequal(f, xs)" begin
    @test allequal(abs2, [3, -3])
    @test allequal(x -> 1, rand(3))
    @test !allequal(x -> rand(), [1,1,1])
    # tuples
    @test allequal(abs2, (3, -3))
    @test allequal(x -> 1, Tuple(rand(3)))
    @test !allequal(x -> rand(), (1,1,1))
    # These cases don't call the function at all:
    @test allequal(error, [])
    @test allequal(error, ())
    @test allequal(error, (x for x in 1:3 if false))
    @test_skip allequal(error, [1])  # fixed not by new code but by upgrades to old code
    @test allequal(error, (1,))
end

# https://github.com/JuliaLang/julia/pull/47354
@testset "cycle(iter, n)"  begin
    using Base.Iterators: cycle
    @test collect(cycle(0:3, 2)) == [0, 1, 2, 3, 0, 1, 2, 3]
    @test collect(cycle(Iterators.filter(iseven, 1:4), 2)) == [2, 4, 2, 4]
    # @test collect(take(cycle(countfrom(11), 3), 4)) == 11:14  # this iterator is defined in Base's tests

    @test isempty(cycle(1:0)) == isempty(cycle(1:0, 3)) == true
    @test isempty(cycle(1:5, 0))
    @test isempty(cycle(Iterators.filter(iseven, 1:4), 0))

    @test eltype(cycle(0:3, 2)) === Int
    @test Base.IteratorEltype(cycle(0:3, 2)) == Base.HasEltype()

    Base.haslength(cycle(0:3, 2)) == false  # but not sure we should test these
    Base.IteratorSize(cycle(0:3, 2)) == Base.SizeUnknown()
end

# https://github.com/JuliaLang/julia/pull/39071
@testset "logrange" begin
    # basic idea
    @test logrange(2, 16, 4) ≈ [2, 4, 8, 16]
    @test logrange(1/8, 8.0, 7) ≈ [0.125, 0.25, 0.5, 1.0, 2.0, 4.0, 8.0]
    @test logrange(1000, 1, 4) ≈ [1000, 100, 10, 1]
    @test logrange(1, 10^9, 19)[1:2:end] ≈ 10 .^ (0:9)

    # endpoints
    @test logrange(0.1f0, 100, 33)[1] === 0.1f0
    @test logrange(0.789, 123_456, 135_790)[[begin, end]] == [0.789, 123_456]
    @test logrange(nextfloat(0f0), floatmax(Float32), typemax(Int))[end] === floatmax(Float32)
    @test logrange(nextfloat(Float16(0)), floatmax(Float16), 66_000)[end] === floatmax(Float16)
    @test first(logrange(pi, 2pi, 3000)) === logrange(pi, 2pi, 3000)[1] === Float64(pi)
    if Int == Int64
        @test logrange(0.1, 1000, 2^54)[end] === 1000.0
    end

    # empty, only, constant
    @test first(logrange(1, 2, 0)) === 1.0
    @test last(logrange(1, 2, 0)) === 2.0
    @test collect(logrange(1, 2, 0)) == Float64[]
    @test only(logrange(2pi, 2pi, 1)) === logrange(2pi, 2pi, 1)[1] === 2pi
    @test logrange(1, 1, 3) == fill(1.0, 3)

    # subnormal Float64
    x = logrange(1e-320, 1e-300, 21) .* 1e300
    @test x ≈ logrange(1e-20, 1, 21) rtol=1e-6

    # types
    @test eltype(logrange(1, 10, 3)) == Float64
    @test eltype(logrange(1, 10, Int32(3))) == Float64
    @test eltype(logrange(1, 10f0, 3)) == Float32
    @test eltype(logrange(1f0, 10, 3)) == Float32
    @test eltype(logrange(1, big(10), 3)) == BigFloat
    @test logrange(big"0.3", big(pi), 50)[1] == big"0.3"
    @test logrange(big"0.3", big(pi), 50)[end] == big(pi)

    # more constructors
    @test logrange(1,2,length=3) === Compat.LogRange(1,2,3) == Compat.LogRange{Float64}(1,2,3)
    @test logrange(1f0, 2f0, length=3) == Compat.LogRange{Float32}(1,2,3)

    # errors
    @test_throws UndefKeywordError logrange(1, 10)  # no default length
    @test_throws ArgumentError logrange(1, 10, -1)  # negative length
    @test_throws ArgumentError logrange(1, 10, 1) # endpoints must not differ
    @test_throws DomainError logrange(1, -1, 3)   # needs complex numbers
    @test_throws DomainError logrange(-1, -2, 3)  # not supported, for now
    @test_throws MethodError logrange(1, 2+3im, length=4)  # not supported, for now
    @test_throws ArgumentError logrange(1, 10, 2)[true]  # bad index
    @test_throws BoundsError logrange(1, 10, 2)[3]
    @test_throws ArgumentError Compat.LogRange{Int}(1,4,5)  # no integer ranges
    @test_throws MethodError Compat.LogRange(1,4, length=5)  # type does not take keyword
    # (not sure if these should ideally be DomainError or ArgumentError)
    @test_throws DomainError logrange(1, Inf, 3)
    @test_throws DomainError logrange(0, 2, 3)
    @test_throws DomainError logrange(1, NaN, 3)
    @test_throws DomainError logrange(NaN, 2, 3)

    # printing
    @test repr(Compat.LogRange(1,2,3)) == "LogRange{Float64}(1.0, 2.0, 3)"  # like 2-arg show
    @test_skip repr("text/plain", Compat.LogRange(1,2,3)) == "3-element Compat.LogRange{Float64, Base.TwicePrecision{Float64}}:\n 1.0, 1.41421, 2.0"
    @test_skip repr("text/plain", Compat.LogRange(1,2,0)) == "LogRange{Float64}(1.0, 2.0, 0)"  # empty case
end

# https://github.com/JuliaLang/julia/pull/45793
@testset "insertdims" begin
     a = rand(8, 7)
     @test @inferred(insertdims(a, dims=1)) == @inferred(insertdims(a, dims=(1,))) == reshape(a, (1, 8, 7))
     @test @inferred(insertdims(a, dims=3))  == @inferred(insertdims(a, dims=(3,))) == reshape(a, (8, 7, 1))
     @test @inferred(insertdims(a, dims=(1, 3)))  == reshape(a, (1, 8, 1, 7))
     @test @inferred(insertdims(a, dims=(1, 2, 3)))  == reshape(a, (1, 1, 1, 8, 7))
     @test @inferred(insertdims(a, dims=(1, 4)))  == reshape(a, (1, 8, 7, 1))
     @test @inferred(insertdims(a, dims=(1, 3, 5)))  == reshape(a, (1, 8, 1, 7, 1))
     @test @inferred(insertdims(a, dims=(1, 2, 4, 6)))  == reshape(a, (1, 1, 8, 1, 7, 1))
     @test @inferred(insertdims(a, dims=(1, 3, 4, 6)))  == reshape(a, (1, 8, 1, 1, 7, 1))
     @test @inferred(insertdims(a, dims=(1, 4, 6, 3)))  == reshape(a, (1, 8, 1, 1, 7, 1))
     @test @inferred(insertdims(a, dims=(1, 3, 5, 6)))  == reshape(a, (1, 8, 1, 7, 1, 1))

     @test_throws ArgumentError insertdims(a, dims=(1, 1, 2, 3))
     @test_throws ArgumentError insertdims(a, dims=(1, 2, 2, 3))
     @test_throws ArgumentError insertdims(a, dims=(1, 2, 3, 3))
     @test_throws UndefKeywordError insertdims(a)
     @test_throws ArgumentError insertdims(a, dims=0)
     @test_throws ArgumentError insertdims(a, dims=(1, 2, 1))
     @test_throws ArgumentError insertdims(a, dims=4)
     @test_throws ArgumentError insertdims(a, dims=6)

     # insertdims and dropdims are inverses
     b = rand(1,1,1,5,1,1,7)
     for dims in [1, (1,), 2, (2,), 3, (3,), (1,3), (1,2,3), (1,2), (1,3,5), (1,2,5,6), (1,3,5,6), (1,3,5,6), (1,6,5,3)]
         @test dropdims(insertdims(a; dims); dims) == a
         @test insertdims(dropdims(b; dims); dims) == b
     end
end
    
# https://github.com/JuliaLang/julia/pull/54653: add Fix
@testset "Fix" begin
    function test_fix1(Fix1=Compat.Fix1)
        increment = Fix1(+, 1)
        @test increment(5) == 6
        @test increment(-1) == 0
        @test increment(0) == 1
        @test map(increment, [1, 2, 3]) == [2, 3, 4]

        concat_with_hello = Fix1(*, "Hello ")
        @test concat_with_hello("World!") == "Hello World!"
        # Make sure inference is good:
        @inferred concat_with_hello("World!")

        one_divided_by = Fix1(/, 1)
        @test one_divided_by(10) == 1/10.0
        @test one_divided_by(-5) == 1/-5.0

        return nothing
    end

    function test_fix2(Fix2=Compat.Fix2)
        return_second = Fix2((x, y) -> y, 999)
        @test return_second(10) == 999
        @inferred return_second(10)
        @test return_second(-5) == 999

        divide_by_two = Fix2(/, 2)
        @test map(divide_by_two, (2, 4, 6)) == (1.0, 2.0, 3.0)
        @inferred map(divide_by_two, (2, 4, 6))

        concat_with_world = Fix2(*, " World!")
        @test concat_with_world("Hello") == "Hello World!"
        @inferred concat_with_world("Hello World!")

        return nothing
    end

    # Test with normal Base.Fix1 and Base.Fix2
    test_fix1()
    test_fix2()

    # Now, repeat the Fix1 and Fix2 tests, but
    # with a Fix lambda function used in their place
    test_fix1((op, arg) -> Compat.Fix{1}(op, arg))
    test_fix2((op, arg) -> Compat.Fix{2}(op, arg))

    # Now, we do more complex tests of Fix:
    let Fix=Compat.Fix
        @testset "Argument Fixation" begin
            let f = (x, y, z) -> x + y * z
                fixed_f1 = Fix{1}(f, 10)
                @test fixed_f1(2, 3) == 10 + 2 * 3

                fixed_f2 = Fix{2}(f, 5)
                @test fixed_f2(1, 4) == 1 + 5 * 4

                fixed_f3 = Fix{3}(f, 3)
                @test fixed_f3(1, 2) == 1 + 2 * 3
            end
        end
        @testset "Helpful errors" begin
            let g = (x, y) -> x - y
                # Test minimum N
                fixed_g1 = Fix{1}(g, 100)
                @test fixed_g1(40) == 100 - 40

                # Test maximum N
                fixed_g2 = Fix{2}(g, 100)
                @test fixed_g2(150) == 150 - 100

                # One over
                fixed_g3 = Fix{3}(g, 100)
                @test_throws ArgumentError("expected at least 2 arguments to `Fix{3}`, but got 1") fixed_g3(1)
            end
        end
        @testset "Type Stability and Inference" begin
            let h = (x, y) -> x / y
                fixed_h = Fix{2}(h, 2.0)
                @test @inferred(fixed_h(4.0)) == 2.0
            end
        end
        @testset "Interaction with varargs" begin
            vararg_f = (x, y, z...) -> x + 10 * y + sum(z; init=zero(x))
            fixed_vararg_f = Fix{2}(vararg_f, 6)

            # Can call with variable number of arguments:
            @test fixed_vararg_f(1, 2, 3, 4) == 1 + 10 * 6 + sum((2, 3, 4))
            if VERSION >= v"1.7.0"
                @inferred fixed_vararg_f(1, 2, 3, 4)
            end
            @test fixed_vararg_f(5) == 5 + 10 * 6
            if VERSION >= v"1.7.0"
                @inferred fixed_vararg_f(5)
            end
        end
        @testset "Errors should propagate normally" begin
            error_f = (x, y) -> sin(x * y)
            fixed_error_f = Fix{2}(error_f, Inf)
            @test_throws DomainError fixed_error_f(10)
        end
        @testset "Chaining Fix together" begin
            f1 = Fix{1}(*, "1")
            f2 = Fix{1}(f1, "2")
            f3 = Fix{1}(f2, "3")
            @test f3() == "123"

            g1 = Fix{2}(*, "1")
            g2 = Fix{2}(g1, "2")
            g3 = Fix{2}(g2, "3")
            @test g3("") == "123"
        end
        @testset "Zero arguments" begin
            f = Fix{1}(x -> x, 'a')
            @test f() == 'a'
        end
        @testset "Dummy-proofing" begin
            @test_throws ArgumentError("expected `N` in `Fix{N}` to be integer greater than 0, but got 0") Fix{0}(>, 1)
            @test_throws ArgumentError("expected type parameter in `Fix` to be `Int`, but got `0.5::Float64`") Fix{0.5}(>, 1)
            @test_throws ArgumentError("expected type parameter in `Fix` to be `Int`, but got `1::UInt64`") Fix{UInt64(1)}(>, 1)
        end
        @testset "Specialize to structs not in `Base`" begin
            struct MyStruct
                x::Int
            end
            f = Fix{1}(MyStruct, 1)
            @test f isa Fix{1,Type{MyStruct},Int}
        end
    end
end
