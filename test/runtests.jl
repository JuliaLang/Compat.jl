using Compat
using Dates
using TOML
using Test

@test isempty(detect_ambiguities(Base, Core, Compat))

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

# https://github.com/JuliaLang/julia/pull/35316
# https://github.com/JuliaLang/julia/pull/41076
@testset "2arg" begin
    @testset "findmin(f, domain)" begin
        @test findmin(-, 1:10) == (-10, 10)
        @test findmin(identity, [1, 2, 3, missing]) === (missing, 4)
        @test findmin(identity, [1, NaN, 3, missing]) === (missing, 4)
        @test findmin(identity, [1, missing, NaN, 3]) === (missing, 2)
        @test findmin(identity, [1, NaN, 3]) === (NaN, 2)
        @test findmin(identity, [1, 3, NaN]) === (NaN, 3)
        @test all(findmin(cos, 0:π/2:2π) .≈ (-1.0, 3))
    end

    @testset "findmax(f, domain)" begin
        @test findmax(-, 1:10) == (-1, 1)
        @test findmax(identity, [1, 2, 3, missing]) === (missing, 4)
        @test findmax(identity, [1, NaN, 3, missing]) === (missing, 4)
        @test findmax(identity, [1, missing, NaN, 3]) === (missing, 2)
        @test findmax(identity, [1, NaN, 3]) === (NaN, 2)
        @test findmax(identity, [1, 3, NaN]) === (NaN, 3)
        @test findmax(cos, 0:π/2:2π) == (1.0, 1)
    end

    @testset "argmin(f, domain)" begin
        @test argmin(-, 1:10) == 10
        @test argmin(sum, Iterators.product(1:5, 1:5)) == (1, 1)
    end

    @testset "argmax(f, domain)" begin
        @test argmax(-, 1:10) == 1
        @test argmax(sum, Iterators.product(1:5, 1:5)) == (5, 5)
    end
end

# https://github.com/JuliaLang/julia/pull/40729
@testset "@something" begin
    @test_throws ArgumentError @something()
    @test_throws ArgumentError @something(nothing)
    @test @something(1) === 1
    @test @something(Some(nothing)) === nothing

    @test @something(1, error("failed")) === 1
    @test_throws ErrorException @something(nothing, error("failed"))
end

@testset "@coalesce" begin
    @test @coalesce() === missing
    @test @coalesce(1) === 1
    @test @coalesce(nothing) === nothing
    @test @coalesce(missing) === missing

    @test @coalesce(1, error("failed")) === 1
    @test_throws ErrorException @coalesce(missing, error("failed"))
end

@testset "get" begin
     A = reshape([1:24...], 4, 3, 2)
     B = reshape([1:24...], 4, 3, 2)

     global c = 0
     f() = (global c = c+1; 0)
     @test get(f, A, ()) == 0
     @test c == 1
     @test get(f, B, ()) == 0
     @test c == 2
     @test get(f, A, (1,)) == get(f, A, 1) == A[1] == 1
     @test c == 2
     @test get(f, B, (1,)) == get(f, B, 1) == B[1] == 1
     @test c == 2
     @test get(f, A, (25,)) == get(f, A, 25) == 0
     @test c == 4
     @test get(f, B, (25,)) == get(f, B, 25) == 0
     @test c == 6
     @test get(f, A, (1,1,1)) == A[1,1,1] == 1
     @test get(f, B, (1,1,1)) == B[1,1,1] == 1
     @test get(f, A, (1,1,3)) == 0
     @test c == 7
     @test get(f, B, (1,1,3)) == 0
     @test c == 8
     @test get(f, TSlow([]), ()) == 0
     @test c == 9

     @test get((5, 6, 7), 1, 0) == 5
     @test get((), 5, 0) == 0
     @test get((1,), 3, 0) == 0
     @test get(()->0, (5, 6, 7), 1) == 5
     @test get(()->0, (), 4) == 0
     @test get(()->0, (1,), 3) == 0

    for x in [1.23, 7, ℯ, 4//5] #[FP, Int, Irrational, Rat]
         @test get(x, 1, 99) == x
         @test get(x, (), 99) == x
         @test get(x, (1,), 99) == x
         @test get(x, 2, 99) == 99
         @test get(x, 0, pi) == pi
         @test get(x, (1,2), pi) == pi
         c = Ref(0)
         @test get(() -> c[]+=1, x, 1) == x
         @test get(() -> c[]+=1, x, ()) == x
         @test get(() -> c[]+=1, x, (1,1,1)) == x
         @test get(() -> c[]+=1, x, 2) == 1
         @test get(() -> c[]+=1, x, -1) == 2
         @test get(() -> c[]+=1, x, (3,2,1)) == 3
    end
end

# https://github.com/JuliaLang/julia/pull/39285
struct X
    x
end
@testset "property destructuring assignment" begin
    nt = (; a=1, b=2, c=3)
    @compat (; c, b) = nt
    @test c == nt.c
    @test b == nt.b

    @compat (; x) = X(1)
    @test x == 1
end

# https://github.com/JuliaLang/julia/pull/29901
@testset "current_exceptions" begin
    # Helper method to retrieve an ExceptionStack that should contain two exceptions,
    # each of which accompanied by a backtrace or `nothing` according to `with_backtraces`.
    function _retrieve_exception_stack(;with_backtraces::Bool)
        exception_stack = try
            try
                # Generate the first exception:
                __not_a_binding__
            catch
                # Catch the first exception, and generate a second exception
                # during what would be handling of the first exception:
                1 ÷ 0
            end
        catch
            # Retrieve an ExceptionStack with both exceptions,
            # and bind `exception_stack` (at the top of this block) thereto:
            current_exceptions(;backtrace=with_backtraces)
        end
        return exception_stack
    end

    excs_with_bts = _retrieve_exception_stack(with_backtraces = true)
    excs_sans_bts = _retrieve_exception_stack(with_backtraces = false)

    # Check that the ExceptionStack with backtraces contains backtraces:
    BACKTRACE_TYPE = Vector{Union{Ptr{Nothing}, Base.InterpreterIP}}
    @test all(exc_with_bt[2] isa BACKTRACE_TYPE for exc_with_bt in excs_with_bts)

    # Check that the ExceptionStack without backtraces contains `nothing`s:
    @test all(exc_sans_bt[2] isa Nothing for exc_sans_bt in excs_sans_bts)

    # Check that the ExceptionStacks contain the expected exception types:
    @test typeof.(first.(excs_with_bts)) == [UndefVarError, DivideError]
    @test typeof.(first.(excs_sans_bts)) == [UndefVarError, DivideError]

    # Check that the ExceptionStack with backtraces `show`s correctly:
    @test occursin(r"""
    2-element ExceptionStack:
    DivideError: integer division error
    Stacktrace:.*

    caused by: UndefVarError: `?__not_a_binding__`? not defined( in `Main`)?
    Stacktrace:.*
    """s, sprint(show, excs_with_bts))

    # Check that the ExceptionStack without backtraces `show`s correctly:
    @test occursin(r"""
    2-element ExceptionStack:
    DivideError: integer division error

    caused by: UndefVarError: `?__not_a_binding__`? not defined"""s,
    sprint(show, excs_sans_bts))

    # Check that the ExceptionStack with backtraces `display_error`s correctly:
    @test occursin(r"""
    ERROR: DivideError: integer division error
    Stacktrace:.*

    caused by: UndefVarError: `?__not_a_binding__`? not defined( in `Main`)?
    Stacktrace:.*
    """s, sprint(Base.display_error, excs_with_bts))

    # Check that the ExceptionStack without backtraces `display_error`s correctly:
    @test occursin(r"""
    ERROR: DivideError: integer division error

    caused by: UndefVarError: `?__not_a_binding__`? not defined"""s,
    sprint(Base.display_error, excs_sans_bts))
end

# https://github.com/JuliaLang/julia/pull/39794
@testset "Returns" begin
    @test @inferred(Returns(1)()   ) === 1
    @test @inferred(Returns(1)(23) ) === 1
    @test @inferred(Returns("a")(2,3)) == "a"
    @test @inferred(Returns(1)(x=1, y=2)) === 1
    @test @inferred(Returns(Int)()) === Int
    @test @inferred(Returns(Returns(1))()) === Returns(1)
    f = @inferred Returns(Int)
    @inferred f(1,2)
    val = [1,2,3]
    @test Returns(val)(1) === val
    @test sprint(show, Returns(1.0)) == "Returns{Float64}(1.0)"
end

# https://github.com/JuliaLang/julia/pull/43852
@testset "@assume_effects" begin
    # ensure proper macro hygiene across versions
    Compat.@assume_effects :total foo() = true
    Compat.@assume_effects bar() = true
    @test foo()
    @test bar()
end

# https://github.com/JuliaLang/julia/pull/42125
@testset "@constprop" begin
    Compat.@constprop :aggressive aggf(x) = Symbol(x)
    Compat.@constprop :none      nonef(x) = Symbol(x)
    @test_throws Exception Meta.lower(@__MODULE__,
        quote
            Compat.@constprop :other brokenf(x) = Symbol(x)
        end
    )
    @test aggf("hi") == nonef("hi") == :hi
end

# https://github.com/JuliaLang/julia/pull/41312
@testset "`@inline`/`@noinline` annotations within a function body" begin
    callf(f, args...) = f(args...)
    function foo1(a)
        Compat.@inline
        sum(sincos(a))
    end
    foo2(a) = (Compat.@inline; sum(sincos(a)))
    foo3(a) = callf(a) do a
        Compat.@inline
        sum(sincos(a))
    end
    function foo4(a)
        Compat.@noinline
        sum(sincos(a))
    end
    foo5(a) = (Compat.@noinline; sum(sincos(a)))
    foo6(a) = callf(a) do a
        Compat.@noinline
        sum(sincos(a))
    end

    @test foo1(42) == foo2(42) == foo3(42) == foo4(42) == foo5(42) == foo6(42)
end

# https://github.com/JuliaLang/julia/pull/41328
@testset "callsite annotations of inlining" begin
    function foo1(a)
        Compat.@inline begin
            return sum(sincos(a))
        end
    end
    function foo2(a)
        Compat.@noinline begin
            return sum(sincos(a))
        end
    end

    @test foo1(42) == foo2(42)
end

# https://github.com/JuliaLang/julia/pull/40803
@testset "Convert CompoundPeriod to Period" begin
    @test convert(Month, Year(1) + Month(1)) === Month(13)
    @test convert(Second, Minute(1) + Second(30)) === Second(90)
    @test convert(Minute, Minute(1) + Second(60)) === Minute(2)
    @test convert(Millisecond, Minute(1) + Second(30)) === Millisecond(90_000)
    @test_throws InexactError convert(Minute, Minute(1) + Second(30))
    @test_throws MethodError convert(Month, Minute(1) + Second(30))
    @test_throws MethodError convert(Second, Month(1) + Second(30))
    @test_throws MethodError convert(Period, Minute(1) + Second(30))
    @test_throws MethodError convert(Dates.FixedPeriod, Minute(1) + Second(30))
end

@testset "ismutabletype" begin
    @test ismutabletype(Array)
    @test !ismutabletype(Tuple)
end

# https://github.com/JuliaLang/julia/pull/39245

#=
cmcaine commented on Sep 8, 2021

This PR implements split with eachsplit and uses eachsplit in a few other places in Base,
so it's kind of already covered by the existing tests.
Not sure it needs any more?

so, these are the Base.split tests, but replacing split with eachsplit |> collect
=#
@testset "eachsplit" begin
    @test eachsplit("foo,bar,baz", 'x') |> collect == ["foo,bar,baz"]
    @test eachsplit("foo,bar,baz", ',') |> collect == ["foo","bar","baz"]
    @test eachsplit("foo,bar,baz", ",") |> collect == ["foo","bar","baz"]
    @test eachsplit("foo,bar,baz", r",") |> collect == ["foo","bar","baz"]
    @test eachsplit("foo,bar,baz", ','; limit=0) |> collect == ["foo","bar","baz"]
    @test eachsplit("foo,bar,baz", ','; limit=1) |> collect == ["foo,bar,baz"]
    @test eachsplit("foo,bar,baz", ','; limit=2) |> collect == ["foo","bar,baz"]
    @test eachsplit("foo,bar,baz", ','; limit=3) |> collect == ["foo","bar","baz"]
    @test eachsplit("foo,bar", "o,b") |> collect == ["fo","ar"]

    @test eachsplit("", ',') |> collect == [""]
    @test eachsplit(",", ',') |> collect == ["",""]
    @test eachsplit(",,", ',') |> collect == ["","",""]
    @test eachsplit("", ','  ; keepempty=false) |> collect == SubString[]
    @test eachsplit(",", ',' ; keepempty=false) |> collect == SubString[]
    @test eachsplit(",,", ','; keepempty=false) |> collect == SubString[]

    @test eachsplit("a b c") |> collect == ["a","b","c"]
    @test eachsplit("a  b \t c\n") |> collect == ["a","b","c"]
    @test eachsplit("α  β \u2009 γ\n") |> collect == ["α","β","γ"]

    @test eachsplit("a b c"; limit=2) |> collect == ["a","b c"]
    @test eachsplit("a  b \t c\n"; limit=3) |> collect == ["a","b","\t c\n"]
    @test eachsplit("a b c"; keepempty=true) |> collect == ["a","b","c"]
    @test eachsplit("a  b \t c\n"; keepempty=true) |> collect == ["a","","b","","","c",""]

    let str = "a.:.ba..:..cba.:.:.dcba.:."
        @test eachsplit(str, ".:.") |> collect == ["a","ba.",".cba",":.dcba",""]
        @test eachsplit(str, ".:."; keepempty=false) |> collect == ["a","ba.",".cba",":.dcba"]
        @test eachsplit(str, ".:.") |> collect == ["a","ba.",".cba",":.dcba",""]
        @test eachsplit(str, r"\.(:\.)+") |> collect == ["a","ba.",".cba","dcba",""]
        @test eachsplit(str, r"\.(:\.)+"; keepempty=false) |> collect == ["a","ba.",".cba","dcba"]
        @test eachsplit(str, r"\.+:\.+") |> collect == ["a","ba","cba",":.dcba",""]
        @test eachsplit(str, r"\.+:\.+"; keepempty=false) |> collect == ["a","ba","cba",":.dcba"]
    end

    # zero-width splits
    @test eachsplit("", "") |> collect == rsplit("", "") == [""]
    @test eachsplit("abc", "") |> collect == rsplit("abc", "") == ["a","b","c"]
    @test eachsplit("abc", "", limit=2)  |> collect == ["a","bc"]

    @test eachsplit("", r"")  |> collect == [""]
    @test eachsplit("abc", r"") |> collect == ["a","b","c"]
    @test eachsplit("abcd", r"b?") |> collect == ["a","c","d"]
    @test eachsplit("abcd", r"b*") |> collect == ["a","c","d"]
    @test eachsplit("abcd", r"b+") |> collect == ["a","cd"]
    @test eachsplit("abcd", r"b?c?") |> collect == ["a","d"]
    @test eachsplit("abcd", r"[bc]?") |> collect == ["a","","d"]
    @test eachsplit("abcd", r"a*") |> collect == ["","b","c","d"]
    @test eachsplit("abcd", r"a+") |> collect == ["","bcd"]
    @test eachsplit("abcd", r"d*") |> collect == ["a","b","c",""]
    @test eachsplit("abcd", r"d+") |> collect == ["abc",""]
    @test eachsplit("abcd", r"[ad]?") |> collect == ["","b","c",""]

    # multi-byte unicode characters (issue #26225)
    @test eachsplit("α β γ", " ") |> collect == rsplit("α β γ", " ") ==
        eachsplit("α β γ", isspace) |> collect == rsplit("α β γ", isspace) == ["α","β","γ"]
    @test eachsplit("ö.", ".") |> collect == rsplit("ö.", ".") == ["ö",""]
    @test eachsplit("α β γ", "β") |> collect == rsplit("α β γ", "β") == ["α "," γ"]
end

# https://github.com/JuliaLang/julia/pull/43354
@testset "allequal" begin
    @test allequal(Set())
    @test allequal(Set(1))
    @test !allequal(Set([1, 2]))
    @test allequal(Dict())
    @test allequal(Dict(:a => 1))
    @test !allequal(Dict(:a => 1, :b => 2))
    @test allequal([])
    @test allequal([1])
    @test allequal([1, 1])
    @test !allequal([1, 1, 2])
    @test allequal([:a, :a])
    @test !allequal([:a, :b])
    @test !allequal(1:2)
    @test allequal(1:1)
    @test !allequal(4.0:0.3:7.0)
    @test allequal(4:-1:5)       # empty range
    @test !allequal(7:-1:1)       # negative step
    @test !allequal(Date(2018, 8, 7):Day(1):Date(2018, 8, 11))  # JuliaCon 2018
    @test !allequal(DateTime(2018, 8, 7):Hour(1):DateTime(2018, 8, 11))
    @test allequal(StepRangeLen(1.0, 0.0, 2))
    @test !allequal(StepRangeLen(1.0, 1.0, 2))
    @test allequal(LinRange(1, 1, 0))
    @test allequal(LinRange(1, 1, 1))
    @test allequal(LinRange(1, 1, 2))
    @test !allequal(LinRange(1, 2, 2))
end

@testset "keepat!" begin
    a = [1:6;]
    @test a === keepat!(a, 1:5)
    @test a == 1:5
    @test keepat!(a, [2, 4]) == [2, 4]
    @test isempty(keepat!(a, []))

    a = [1:6;]
    @test_throws BoundsError keepat!(a, 1:10) # make sure this is not a no-op
    @test_throws BoundsError keepat!(a, 2:10)
    @test_throws ArgumentError keepat!(a, [2, 4, 3])

    b = BitVector([1, 1, 1, 0, 0])
    @test b === keepat!(b, 1:5)
    @test b == [1, 1, 1, 0, 0]
    @test keepat!(b, 2:4) == [1, 1, 0]
    @test_throws BoundsError keepat!(a, -1:10)
    @test_throws ArgumentError keepat!(a, [2, 1])
    @test isempty(keepat!(a, []))

    # Vector
    a = Vector(1:10)
    keepat!(a, [falses(5); trues(5)])
    @test a == 6:10
    @test_throws BoundsError keepat!(a, trues(1))
    @test_throws BoundsError keepat!(a, trues(11))

    # BitVector
    ba = rand(10) .> 0.5
    @test isa(ba, BitArray)
    keepat!(ba, ba)
    @test all(ba)

    # empty array
    ea = []
    keepat!(ea, Bool[])
    @test isempty(ea)
end

@testset "pkgversion" begin
    toml = joinpath(pkgdir(Compat), "Project.toml")
    @test pkgversion(Compat) == VersionNumber(TOML.parsefile(toml)["version"])
    @test pkgversion(Base) === nothing
end

# https://github.com/JuliaLang/julia/pull/43334
@testset "stack" begin
    # Basics
    for args in ([[1, 2]], [1:2, 3:4], [[1 2; 3 4], [5 6; 7 8]],
                AbstractVector[1:2, [3.5, 4.5]], Vector[[1,2], [3im, 4im]],
                [[1:2, 3:4], [5:6, 7:8]], [fill(1), fill(2)])
        X = stack(args)
        Y = cat(args...; dims=ndims(args[1])+1)
        @test X == Y
        @test typeof(X) === typeof(Y)

        X2 = stack(x for x in args)
        @test X2 == Y
        @test typeof(X2) === typeof(Y)

        X3 = stack(x for x in args if true)
        @test X3 == Y
        @test typeof(X3) === typeof(Y)

        if isconcretetype(eltype(args))
            @inferred stack(args)
            @inferred stack(x for x in args)
        end
    end

    # Higher dims
    @test size(stack([rand(2,3) for _ in 1:4, _ in 1:5])) == (2,3,4,5)
    @test size(stack(rand(2,3) for _ in 1:4, _ in 1:5)) == (2,3,4,5)
    @test size(stack(rand(2,3) for _ in 1:4, _ in 1:5 if true)) == (2, 3, 20)
    @test size(stack([rand(2,3) for _ in 1:4, _ in 1:5]; dims=1)) == (20, 2, 3)
    @test size(stack(rand(2,3) for _ in 1:4, _ in 1:5; dims=2)) == (2, 20, 3)

    # Tuples
    @test stack([(1,2), (3,4)]) == [1 3; 2 4]
    @test stack(((1,2), (3,4))) == [1 3; 2 4]
    @test stack(Any[(1,2), (3,4)]) == [1 3; 2 4]
    @test stack([(1,2), (3,4)]; dims=1) == [1 2; 3 4]
    @test stack(((1,2), (3,4)); dims=1) == [1 2; 3 4]
    @test stack(Any[(1,2), (3,4)]; dims=1) == [1 2; 3 4]
    @test size(@inferred stack(Iterators.product(1:3, 1:4))) == (2,3,4)
    @test @inferred(stack([('a', 'b'), ('c', 'd')])) == ['a' 'c'; 'b' 'd']
    @test @inferred(stack([(1,2+3im), (4, 5+6im)])) isa Matrix{Number}

    # stack(f, iter)
    @test @inferred(stack(x -> [x, 2x], 3:5)) == [3 4 5; 6 8 10]
    @test @inferred(stack(x -> x*x'/2, [1:2, 3:4])) == reshape([0.5, 1.0, 1.0, 2.0, 4.5, 6.0, 6.0, 8.0], 2, 2, 2)
    @test @inferred(stack(*, [1:2, 3:4], 5:6)) == [5 18; 10 24]

    # Iterators
    @test stack([(a=1,b=2), (a=3,b=4)]) == [1 3; 2 4]
    @test stack([(a=1,b=2), (c=3,d=4)]) == [1 3; 2 4]
    @test stack([(a=1,b=2), (c=3,d=4)]; dims=1) == [1 2; 3 4]
    @test stack([(a=1,b=2), (c=3,d=4)]; dims=2) == [1 3; 2 4]
    @test stack((x/y for x in 1:3) for y in 4:5) == (1:3) ./ (4:5)'
    @test stack((x/y for x in 1:3) for y in 4:5; dims=1) == (1:3)' ./ (4:5)

    # Exotic
    ips = ((Iterators.product([i,i^2], [2i,3i,4i], 1:4)) for i in 1:5)
    @test size(stack(ips)) == (2, 3, 4, 5)
    @test stack(ips) == cat(collect.(ips)...; dims=4)
    ips_cat2 = cat(reshape.(collect.(ips), Ref((2,1,3,4)))...; dims=2)
    @test stack(ips; dims=2) == ips_cat2
    @test stack(collect.(ips); dims=2) == ips_cat2
    ips_cat3 = cat(reshape.(collect.(ips), Ref((2,3,1,4)))...; dims=3)
    @test stack(ips; dims=3) == ips_cat3  # path for non-array accumulation on non-final dims
    @test stack(collect, ips; dims=3) == ips_cat3  # ... and for array accumulation
    @test stack(collect.(ips); dims=3) == ips_cat3

    # Trivial, because numbers are iterable:
    @test stack(abs2, 1:3) == [1, 4, 9] == collect(Iterators.flatten(abs2(x) for x in 1:3))

    # Allocation tests
    xv = [rand(10) for _ in 1:100]
    xt = Tuple.(xv)
    for dims in (1, 2, :)
        @test stack(xv; dims) == stack(xt; dims)
        @test_skip 9000 > @allocated stack(xv; dims)
        @test_skip 9000 > @allocated stack(xt; dims)
    end
    xr = (reshape(1:1000,10,10,10) for _ = 1:1000)
    for dims in (1, 2, 3, :)
        stack(xr; dims)
        @test_skip 8.1e6 > @allocated stack(xr; dims)
    end

    # Mismatched sizes
    @test_throws DimensionMismatch stack([1:2, 1:3])
    @test_throws DimensionMismatch stack([1:2, 1:3]; dims=1)
    @test_throws DimensionMismatch stack([1:2, 1:3]; dims=2)
    @test_throws DimensionMismatch stack([(1,2), (3,4,5)])
    @test_throws DimensionMismatch stack([(1,2), (3,4,5)]; dims=1)
    @test_throws DimensionMismatch stack(x for x in [1:2, 1:3])
    @test_throws DimensionMismatch stack([[5 6; 7 8], [1, 2, 3, 4]])
    @test_throws DimensionMismatch stack([[5 6; 7 8], [1, 2, 3, 4]]; dims=1)
    @test_throws DimensionMismatch stack(x for x in [[5 6; 7 8], [1, 2, 3, 4]])
    # Inner iterator of unknown length
    @test_throws MethodError stack((x for x in 1:3 if true) for _ in 1:4)
    @test_throws MethodError stack((x for x in 1:3 if true) for _ in 1:4; dims=1)

    @test_throws ArgumentError stack([1:3, 4:6]; dims=0)
    @test_throws ArgumentError stack([1:3, 4:6]; dims=3)
    @test_throws ArgumentError stack(abs2, 1:3; dims=2)

    # Empty
    @test_throws ArgumentError stack(())
    @test_throws ArgumentError stack([])
    @test_throws ArgumentError stack(x for x in 1:3 if false)
end

@testset "promoting ops for TimePeriod" begin
    x = 10
    y = 3
    xs = Second(x)
    ys = Second(y)
    xms = Millisecond(xs)
    yms = Millisecond(ys)
    for op in (/, div, rem, mod, lcm, gcd)
        @test op(xms, yms) == op(xms, ys) == op(xs, yms)
    end
end

@testset "splat" begin
    @test splat(+)((1,2,3)) == 6

    if v"1.10.0-" <= VERSION < v"1.10.0-DEV.360" ||
        v"1.9.0-DEV.513" <= VERSION < v"1.9.0-beta3"
        # these versions of Base export Splat (which we use) but pretty-print with capital `S`
        @test repr(splat(+)) == "Splat(+)"
        @test repr(MIME"text/plain"(), splat(+)) == "Splat(+)"
    else
        @test repr(splat(+)) == "splat(+)"
        @test repr(MIME"text/plain"(), splat(+)) == "splat(+)"
    end
end

# https://github.com/JuliaLang/julia/pull/25085
@testset "Bool rounding (#25074)" begin
    @testset "round Bool" begin
        @test_throws InexactError round(Bool, -4.1)
        @test_throws InexactError round(Bool, 1.5)
        @test true == round(Bool, 1.0)
        @test false == round(Bool, 0.0)
        @test true == round(Bool, 0.6)
        @test false == round(Bool, 0.4)
        @test false == round(Bool, 0.5)
        @test false == round(Bool, -0.5)
    end

    @testset "trunc Bool" begin
        @test_throws InexactError trunc(Bool, -4.1)
        @test_throws InexactError trunc(Bool, 2.5)
        @test true == trunc(Bool, 1.0)
        @test false == trunc(Bool, 0.0)
        @test false == trunc(Bool, 0.6)
        @test false == trunc(Bool, 0.4)
        @test true == trunc(Bool, 1.8)
        @test false == trunc(Bool, -0.5)
    end

    @testset "floor Bool" begin
        @test_throws InexactError floor(Bool, -0.1)
        @test_throws InexactError floor(Bool, 2.5)
        @test true == floor(Bool, 1.0)
        @test false == floor(Bool, 0.0)
        @test false == floor(Bool, 0.6)
        @test true == floor(Bool, 1.8)
    end

    @testset "ceil Bool" begin
        @test_throws InexactError ceil(Bool, -1.4)
        @test_throws InexactError ceil(Bool, 1.5)
        @test true == ceil(Bool, 1.0)
        @test false == ceil(Bool, 0.0)
        @test true == ceil(Bool, 0.6)
        @test false == ceil(Bool, -0.7)
    end
end

# https://github.com/JuliaLang/julia/pull/37978
@testset "redirect_stdio" begin
    randstring() = join(rand('a':'z', 10))

    function hello_err_out()
        println(stderr, "hello from stderr")
        println(stdout, "hello from stdout")
    end
    @testset "same path for multiple streams" begin
        @test_throws ArgumentError redirect_stdio(hello_err_out,
                                            stdin="samepath.txt", stdout="samepath.txt")
        @test_throws ArgumentError redirect_stdio(hello_err_out,
                                            stdin="samepath.txt", stderr="samepath.txt")

        @test_throws ArgumentError redirect_stdio(hello_err_out,
                                            stdin=joinpath("tricky", "..", "samepath.txt"),
                                            stderr="samepath.txt")
        mktempdir() do dir
            path = joinpath(dir, "stdouterr.txt")
            redirect_stdio(hello_err_out, stdout=path, stderr=path)
            @test read(path, String) == """
            hello from stderr
            hello from stdout
            """
        end
    end

    mktempdir() do dir
        path_stdout = joinpath(dir, "stdout.txt")
        path_stderr = joinpath(dir, "stderr.txt")
        redirect_stdio(hello_err_out, stderr=devnull, stdout=path_stdout)
        @test read(path_stdout, String) == "hello from stdout\n"

        open(path_stderr, "w") do ioerr
            redirect_stdio(hello_err_out, stderr=ioerr, stdout=devnull)
        end
        @test read(path_stderr, String) == "hello from stderr\n"
    end

    mktempdir() do dir
        path_stderr = joinpath(dir, "stderr.txt")
        path_stdin  = joinpath(dir, "stdin.txt")
        path_stdout = joinpath(dir, "stdout.txt")

        content_stderr = randstring()
        content_stdout = randstring()

        redirect_stdio(stdout=path_stdout, stderr=path_stderr) do
            print(content_stdout)
            print(stderr, content_stderr)
        end

        @test read(path_stderr, String) == content_stderr
        @test read(path_stdout, String) == content_stdout
    end
end

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

# https://github.com/JuliaLang/julia/pull/45052
@testset "VersionNumber no-op constructor" begin
    v = VersionNumber("1.2.3")
    @test VersionNumber(v) === v
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
