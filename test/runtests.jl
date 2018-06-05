using Compat
using Compat.Test
using Compat.LinearAlgebra
using Compat.SparseArrays

@test isempty(detect_ambiguities(Base, Core, Compat))

const struct_sym = VERSION < v"0.7.0-DEV.1263" ? :type : :struct

# Issue #291
# 0.6
@test (1, 2) == @compat abs.((1, -2))
@test broadcast(+, (1.0, 1.0), (0, -2.0)) == (1.0,-1.0)

# Test for `take!(::Task)`/`take!(::Channel)`
# 0.6
dirwalk = mktempdir()
cd(dirwalk) do
    for i=1:2
        mkdir("sub_dir$i")
        open("file$i", "w") do f end

        mkdir(joinpath("sub_dir1", "subsub_dir$i"))
        touch(joinpath("sub_dir1", "file$i"))
    end
    touch(joinpath("sub_dir2", "file_dir2"))
    has_symlinks = Compat.Sys.isunix() ? true : (isdefined(Base, :WINDOWS_VISTA_VER) && Base.windows_version() >= Base.WINDOWS_VISTA_VER)
    follow_symlink_vec = has_symlinks ? [true, false] : [false]
    has_symlinks && symlink(abspath("sub_dir2"), joinpath("sub_dir1", "link"))
    for follow_symlinks in follow_symlink_vec
        chnl = walkdir(".", follow_symlinks=follow_symlinks)
        root, dirs, files = take!(chnl)
        @test root == "."
        @test dirs == ["sub_dir1", "sub_dir2"]
        @test files == ["file1", "file2"]

        root, dirs, files = take!(chnl)
        @test root == joinpath(".", "sub_dir1")
        @test dirs == (has_symlinks ? ["link", "subsub_dir1", "subsub_dir2"] : ["subsub_dir1", "subsub_dir2"])
        @test files == ["file1", "file2"]

        root, dirs, files = take!(chnl)
        if follow_symlinks
            @test root == joinpath(".", "sub_dir1", "link")
            @test dirs == []
            @test files == ["file_dir2"]
            root, dirs, files = take!(chnl)
        end
        for i=1:2
            @test root == joinpath(".", "sub_dir1", "subsub_dir$i")
            @test dirs == []
            @test files == []
            root, dirs, files = take!(chnl)
        end

        @test root == joinpath(".", "sub_dir2")
        @test dirs == []
        @test files == ["file_dir2"]
    end

    for follow_symlinks in follow_symlink_vec
        chnl = walkdir(".", follow_symlinks=follow_symlinks, topdown=false)
        root, dirs, files = take!(chnl)
        if follow_symlinks
            @test root == joinpath(".", "sub_dir1", "link")
            @test dirs == []
            @test files == ["file_dir2"]
            root, dirs, files = take!(chnl)
        end
        for i=1:2
            @test root == joinpath(".", "sub_dir1", "subsub_dir$i")
            @test dirs == []
            @test files == []
            root, dirs, files = take!(chnl)
        end
        @test root == joinpath(".", "sub_dir1")
        @test dirs ==  (has_symlinks ? ["link", "subsub_dir1", "subsub_dir2"] : ["subsub_dir1", "subsub_dir2"])
        @test files == ["file1", "file2"]

        root, dirs, files = take!(chnl)
        @test root == joinpath(".", "sub_dir2")
        @test dirs == []
        @test files == ["file_dir2"]

        root, dirs, files = take!(chnl)
        @test root == "."
        @test dirs == ["sub_dir1", "sub_dir2"]
        @test files == ["file1", "file2"]
    end
    #test of error handling
    chnl_error = walkdir(".")
    chnl_noerror = walkdir(".", onerror=x->x)
    root, dirs, files = take!(chnl_error)
    @test root == "."
    @test dirs == ["sub_dir1", "sub_dir2"]
    @test files == ["file1", "file2"]

    rm(joinpath("sub_dir1"), recursive=true)
    @test_throws SystemError take!(chnl_error) # throws an error because sub_dir1 do not exist

    root, dirs, files = take!(chnl_noerror)
    @test root == "."
    @test dirs == ["sub_dir1", "sub_dir2"]
    @test files == ["file1", "file2"]

    root, dirs, files = take!(chnl_noerror) # skips sub_dir1 as it no longer exist
    @test root == joinpath(".", "sub_dir2")
    @test dirs == []
    @test files == ["file_dir2"]

end
rm(dirwalk, recursive=true)

let
    # Subset of tests copied from base test/error.jl
    function foo_error(c, n)
        c[1] += 1
        if c[1] <= n
            error("foo")
        end
        return 7
    end

    # Success on first attempt
    c = [0]
    @test Compat.retry(foo_error)(c, 0) == 7
    @test c[1] == 1

    # Success on second attempt
    c = [0]
    @test Compat.retry(foo_error)(c,1) == 7
    @test c[1] == 2

    # 2 failed retry attempts, so exception is raised
    c = [0]
    ex = try
        Compat.retry(foo_error, delays=Compat.ExponentialBackOff(n=2))(c, 3)
    catch e
        e
    end

    c = [0]
    ex = try
        Compat.retry(
            foo_error,
            check=(s,e)->(s, try e.http_status_code == "503" end != true)
        )(c, 2)
    catch e
        e
    end
    @test typeof(ex) == ErrorException
    @test ex.msg == "foo"
    @test c[1] == 2

    # Functions with keyword arguments
    foo_kwargs(x; y=5) = x + y
    @test Compat.retry(foo_kwargs)(3) == 8
    @test Compat.retry(foo_kwargs)(3; y=4) == 7
end

for os in [:apple, :bsd, :linux, :unix, :windows]
    from_base = if VERSION >= v"0.7.0-DEV.914"
        Expr(:., Expr(:., :Base, Base.Meta.quot(:Sys)), Base.Meta.quot(Symbol("is", os)))
    else
        Expr(:., :Base, Base.Meta.quot(Symbol("is_", os)))
    end
    @eval @test Compat.Sys.$(Symbol("is", os))() == $from_base()
end

# do-block redirect_std*
# 0.6
let filename = tempname()
    ret = open(filename, "w") do f
        redirect_stdout(f) do
            println("hello")
            [1,3]
        end
    end
    @test ret == [1,3]
    @test chomp(read(filename, String)) == "hello"
    ret = open(filename, "w") do f
        redirect_stderr(f) do
            println(stderr, "WARNING: hello")
            [2]
        end
    end
    @test ret == [2]
    @test occursin("WARNING: hello", read(filename, String))
    ret = open(filename) do f
        redirect_stdin(f) do
            readline()
        end
    end
    @test occursin("WARNING: hello", ret)
    rm(filename)
end

# 0.6
@test @__DIR__() == dirname(@__FILE__)

# PR #17302
# To be removed when 0.6 support is dropped.
f17302(a::Number) = a
f17302(a::Number, b::Number) = a + b
Compat.@dep_vectorize_1arg Real f17302
Compat.@dep_vectorize_2arg Real f17302
@test_throws MethodError f17302([1im])
@test_throws MethodError f17302([1im], [1im])
@static if VERSION ≥ v"0.7.0-DEV.2988"
    @test (@test_logs (:warn, "`f17302(x::(Base.AbstractArray){T}) where T <: Real` is deprecated, use `f17302.(x)` instead.") f17302([1.0])) == [1.0]
    @test (@test_logs (:warn, "`f17302(x::Real, y::(Base.AbstractArray){T1}) where T1 <: Real` is deprecated, use `f17302.(x, y)` instead.") f17302(1.0, [1])) == [2.0]
    @test (@test_logs (:warn, "`f17302(x::(Base.AbstractArray){T1}, y::Real) where T1 <: Real` is deprecated, use `f17302.(x, y)` instead.") f17302([1.0], 1)) == [2.0]
    @test (@test_logs (:warn, "`f17302(x::(Base.AbstractArray){T1}, y::(Base.AbstractArray){T2}) where {T1 <: Real, T2 <: Real}` is deprecated, use `f17302.(x, y)` instead.") f17302([1.0], [1])) == [2.0]
else
    mktemp() do fname, f
        redirect_stderr(f) do
            @test f17302([1.0]) == [1.0]
            @test f17302(1.0, [1]) == [2.0]
            @test f17302([1.0], 1) == [2.0]
            @test f17302([1.0], [1]) == [2.0]
        end
    end
end

if VERSION < v"0.7.0-DEV.3017"
    types = [
        Bool,
        Float16,
        Float32,
        Float64,
        Int128,
        Int16,
        Int32,
        Int64,
        Int8,
        UInt16,
        UInt32,
        UInt64,
        UInt8,
    ]
    for T in types
        # julia#18510, Nullable constructors
        x = @compat Nullable(one(T), true)
        @test isnull(x) === false
        @test isa(x.value, T)
        @test eltype(x) === T

        x = @compat Nullable{T}(one(T), true)
        y = @compat Nullable{Any}(one(T), true)
        @test isnull(x) === false
        @test isnull(y) === false
        @test isa(x.value, T)
        @test eltype(x) === T
        @test eltype(y) === Any

        x = @compat Nullable{T}(one(T), false)
        y = @compat Nullable{Any}(one(T), false)
        @test isnull(x) === true
        @test isnull(y) === true
        @test eltype(x) === T
        @test eltype(y) === Any

        x = @compat Nullable(one(T), false)
        @test isnull(x) === true
        @test eltype(x) === T

        x = @compat Nullable{T}()
        @test isnull(x) === true
        @test eltype(x) === T

        # julia#18484, generic isnull, unsafe_get
        a = one(T)
        x = @compat Nullable(a, true)
        @test isequal(unsafe_get(x), a)

        x = @compat Nullable{Array{T}}()
        @test_throws UndefRefError unsafe_get(x)
    end
end

@test xor(1,5) == 4
@test 1 ⊻ 5 == 4

# julia#20414
@compat let T = Array{<:Real}, f(x::AbstractVector{<:Real}) = 1
    @test isa([3,4],T)
    @test !isa([3,4im],T)
    @test f(1:3) == f([1,2]) == 1
end
@compat let T = Array{>:Integer}, f(x::AbstractVector{>:Integer}) = 1
    @test isa(Integer[1,2],T)
    @test !isa([3,4],T)
    @test !isa([3.0,4.0],T)
    @test f(Integer[1,2]) == f([1,'a',:sym]) == 1
end

# supertype operator
@test !(Int >: Integer)
@test Integer >: Int

# julia#19246
@test numerator(1//2) === 1
@test denominator(1//2) === 2

# julia#19088
let io = IOBuffer()
    write(io, "aaa")
    @test take!(io) == UInt8['a', 'a', 'a']
    write(io, "bbb")
    @test String(take!(io)) == "bbb"
end

# julia#17510
let x = [1,2,3]
    @compat x .= [3,4,5]
    @test x == [3,4,5]
    @compat x .= x .== 4
    @test x == [0,1,0]
    @compat x .= 7
    @test x == [7,7,7]
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

# julia#19950, tests from Base (#20028)
for T in (Float16, Float32, Float64, BigFloat, Int8, Int16, Int32, Int64, Int128,
          BigInt, UInt8, UInt16, UInt32, UInt64, UInt128)
    @test iszero(T(0))
    @test iszero(Complex{T}(0))
    if T<:Integer
        @test iszero(Rational{T}(0))
    end
    if T<:AbstractFloat
        @test iszero(T(-0.0))
        @test iszero(Complex{T}(-0.0))
    end
end
@test !iszero([0, 1, 2, 3])
@test iszero([0, 0, 0, 0])

let
    x = view(1:10, 2:4)
    D = Diagonal(x)
    @test D[1,1] == 2
    @test D[3,3] == 4
    A = view(rand(5,5), 1:3, 1:3)
    @test D*A == Diagonal(copy(x)) * copy(A)
    @test A*D == copy(A) * Diagonal(copy(x))
end

# julia#17623
# 0.6
@test [true, false] .& [true, true] == [true, false]
@test [true, false] .| [true, true] == [true, true]

# julia#20022
@test !Compat.isapprox(NaN, NaN)
@test Compat.isapprox(NaN, NaN, nans=true)

# julia#13998
for x in (3.1, -17, 3//4, big(111.1), Inf)
    local x
    @test min(x) == max(x) == x
    @test minmax(x) == (x, x)
end

# julia#20006
@compat abstract type AbstractFoo20006 end
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

# @view and @views tests copied from Base
let X = reshape(1:24,2,3,4), Y = 4:-1:1
    @test isa(@view(X[1:3]), SubArray)

    @test X[1:end] == @dotcompat (@view X[1:end]) # test compatibility of @. and @view
    @test X[1:end-3] == @view X[1:end-3]
    @test X[1:end,2,2] == @view X[1:end,2,2]
    @test reshape(X[1,2,1:end-2],2) == @view X[1,2,1:end-2]
    @test reshape(X[1,2,Y[2:end]],3) == @view X[1,2,Y[2:end]]
    @test reshape(X[1:end,2,Y[2:end]],2,3) == @view X[1:end,2,Y[2:end]]

    u = (1,2:3)
    @test reshape(X[u...,2:end],2,3) == @view X[u...,2:end]
    @test reshape(X[(1,)...,(2,)...,2:end],3) == @view X[(1,)...,(2,)...,2:end]

    # the following tests fail on 0.5 because of bugs in the 0.5 Base.@view
    # macro (a bugfix is scheduled to be backported from 0.6: julia#20247)
    if !isdefined(Base, Symbol("@view")) || VERSION ≥ v"0.6.0-dev.2406"
        # test macro hygiene
        let size=(x,y)-> error("should not happen"), Base=nothing
            @test X[1:end,2,2] == @view X[1:end,2,2]
        end

        # test that side effects occur only once
        let foo = typeof(X)[X]
            @test X[2:end-1] == @view (push!(foo,X)[1])[2:end-1]
            @test foo == typeof(X)[X, X]
        end
    end

    # test @views macro
    @views @compat let f!(x) = x[1:end-1] .+= x[2:end].^2
        x = [1,2,3,4]
        f!(x)
        @test x == [5,11,19,4]
        @test isa(x[1:3],SubArray)
        @test x[2] === 11
        @test Dict((1:3) => 4)[1:3] === 4
        x[1:2] .= 0
        @test x == [0,0,19,4]
        x[1:2] .= 5:6
        @test x == [5,6,19,4]
        f!(x[3:end])
        @test x == [5,6,35,4]
        x[Y[2:3]] .= 7:8
        @test x == [5,8,7,4]
        @dotcompat x[([3],)..., ()...] += 3 # @. should convert to .+=, test compatibility with @views
        @test x == [5,8,10,4]
        i = Int[]
        # test that lhs expressions in update operations are evaluated only once:
        x[push!(i,4)[1]] += 5
        @test x == [5,8,10,9] && i == [4]
        x[push!(i,3)[end]] += 2
        @test x == [5,8,12,9] && i == [4,3]
        @dotcompat x[3:end] = 0       # make sure @. works with end expressions in @views
        @test x == [5,8,0,0]
    end
    # same tests, but make sure we can switch the order of @compat and @views
    @compat @views let f!(x) = x[1:end-1] .+= x[2:end].^2
        x = [1,2,3,4]
        f!(x)
        @test x == [5,11,19,4]
        @test isa(x[1:3],SubArray)
        @test x[2] === 11
        @test Dict((1:3) => 4)[1:3] === 4
        x[1:2] .= 0
        @test x == [0,0,19,4]
        x[1:2] .= 5:6
        @test x == [5,6,19,4]
        f!(x[3:end])
        @test x == [5,6,35,4]
        x[Y[2:3]] .= 7:8
        @test x == [5,8,7,4]
        @dotcompat x[([3],)..., ()...] += 3 # @. should convert to .+=, test compatibility with @views
        @test x == [5,8,10,4]
        i = Int[]
        # test that lhs expressions in update operations are evaluated only once:
        x[push!(i,4)[1]] += 5
        @test x == [5,8,10,9] && i == [4]
        x[push!(i,3)[end]] += 2
        @test x == [5,8,12,9] && i == [4,3]
        @dotcompat x[3:end] = 0       # make sure @. works with end expressions in @views
        @test x == [5,8,0,0]
    end
    @views @test isa(X[1:3], SubArray)
    @test X[1:end] == @views X[1:end]
    @test X[1:end-3] == @views X[1:end-3]
    @test X[1:end,2,2] == @views X[1:end,2,2]
    @test reshape(X[1,2,1:end-2],2) == @views X[1,2,1:end-2]
    @test reshape(X[1,2,Y[2:end]],3) == @views X[1,2,Y[2:end]]
    @test reshape(X[1:end,2,Y[2:end]],2,3) == @views X[1:end,2,Y[2:end]]
    @test reshape(X[u...,2:end],2,3) == @views X[u...,2:end]
    @test reshape(X[(1,)...,(2,)...,2:end],3) == @views X[(1,)...,(2,)...,2:end]

    # test macro hygiene
    let size=(x,y)-> error("should not happen"), Base=nothing
        @test X[1:end,2,2] == @views X[1:end,2,2]
    end
end

# @. (@__dot__) tests, from base:
let x = [4, -9, 1, -16]
    @test [2, 3, 4, 5] == @dotcompat(1 + sqrt($sort(abs(x))))
    @test @dotcompat(x^2) == x.^2
    @dotcompat x = 2
    @test x == [2,2,2,2]
end
@test [1,4,9] == @dotcompat let x = [1,2,3]; x^2; end
let x = [1,2,3], y = x
    @dotcompat for i = 1:3
        y = y^2 # should convert to y .= y.^2
    end
    @test x == [1,256,6561]
end
let x = [1,2,3]
    @dotcompat f(x) = x^2
    @test f(x) == [1,4,9]
end

# PR #20418
@compat abstract type Abstract20418{T} <: Ref{T} end
@test Compat.TypeUtils.isabstract(Abstract20418)
@compat primitive type Primitive20418{T} <: Ref{T} 16 end
@test !Compat.TypeUtils.isabstract(Primitive20418)
@test isbitstype(Primitive20418{Int})
@test sizeof(Primitive20418{Int}) == 2

# PR #20500
@compat A20500{T<:Integer} = Array{T,20500}
@compat const A20500_2{T<:Union{Int,Float32}} = Pair{T,T}
f20500() = A20500
f20500_2() = A20500_2
@inferred f20500()
@inferred f20500_2()

module CompatArray
    using Compat
    const struct_sym = VERSION < v"0.7.0-DEV.1263" ? :type : :struct
    eval(Expr(
        struct_sym, false,
        Expr(:(<:), :(CartesianArray{T,N}), :(AbstractArray{T,N})),
        quote
            parent::Array{T,N}
        end))
    eval(Expr(
        struct_sym, false,
        Expr(:(<:), :(LinearArray{T,N}), :(AbstractArray{T,N})),
        quote
            parent::Array{T,N}
        end))
    @compat Base.IndexStyle(::Type{<:LinearArray}) = IndexLinear()
end
@test IndexStyle(Array{Float32,2}) === IndexLinear()
@test IndexStyle(CompatArray.CartesianArray{Float32,2}) === IndexCartesian()
@test IndexStyle(CompatArray.LinearArray{Float32,2}) === IndexLinear()
let a = CompatArray.CartesianArray(rand(2,3)), b = CompatArray.LinearArray(rand(2,3))
    @test IndexStyle(a) === IndexCartesian()
    @test IndexStyle(b) === IndexLinear()
end

if VERSION < v"0.6.0-dev.1653"
    for (A,val) in ((zeros(1:5, Float32, 3, 2), 0),
                    (ones(1:5, Float32, 3, 2), 1),
                    (zeros(1:5, Float32, (3, 2)), 0),
                    (ones(1:5, Float32, (3, 2)), 1))
        @test isa(A, Matrix{Float32}) && size(A) == (3,2) && all(x->x==val, A)
    end
    for (A,val) in ((zeros(1:5, Float32), 0),
                    (ones(1:5, Float32), 1))
        @test isa(A, Vector{Float32}) && size(A) == (5,) && all(x->x==val, A)
    end
end

# PR 20203
@test Compat.readline(IOBuffer("Hello, World!\n")) == "Hello, World!"
@test Compat.readline(IOBuffer("x\n"), keep=false) == "x"
@test Compat.readline(IOBuffer("x\n"), keep=true) == "x\n"
@test collect(Compat.eachline(IOBuffer("x\ny"))) == ["x", "y"]
@test collect(Compat.eachline(IOBuffer("x\ny"), keep=false)) == ["x", "y"]
@test collect(Compat.eachline(IOBuffer("x\ny"), keep=true))  == ["x\n", "y"]

# PR 18727
let
    iset = Set([17, 4711])
    cfset = convert(Set{Float64}, iset)
    @test typeof(cfset) == Set{Float64}
    @test cfset == iset
    fset = Set([17.0, 4711.0])
    ciset = convert(Set{Int}, fset)
    @test typeof(ciset) == Set{Int}
    @test ciset == fset
    ssset = Set(split("foo bar"))
    cssset = convert(Set{String}, ssset)
    @test typeof(cssset) == Set{String}
    @test cssset == Set(["foo", "bar"])
end

# PR 18082
@test !isassigned(Ref{String}())
@test isassigned(Ref{String}("Test"))

@test unsafe_trunc(Int8, 128) === Int8(-128)
@test_throws InexactError trunc(Int8, 128)

# PR 21346
let zbuf = IOBuffer([0xbf, 0xc0, 0x00, 0x00, 0x40, 0x20, 0x00, 0x00,
                     0x40, 0x0c, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                     0xc0, 0x12, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])
    z1 = read(zbuf, ComplexF32)
    z2 = read(zbuf, ComplexF64)
    @test bswap(z1) === -1.5f0 + 2.5f0im
    @test bswap(z2) ===  3.5 - 4.5im
end

# PR 19449
using Compat: StringVector
@test length(StringVector(5)) == 5
@test String(fill!(StringVector(5), 0x61)) == "aaaaa"

# collect
if VERSION < v"0.7.0-"
    # Note: This is disabled on 0.7, since the Compat.collect functionality is only
    # applicable on 0.5, and OffsetArrays currently has some incompatibilities with
    # 0.7. This can be reenabled later if needed.
    using OffsetArrays
    a = OffsetArray(1:3, -1:1)
    b = Compat.collect(a)
    @test indices(b) === (Base.OneTo(3),)
    @test b == [1,2,3]
end

# PR 22064
module Test22064
using Compat
using Compat.Test
@test (@__MODULE__) === Test22064
end

# invokelatest
issue19774(x) = 1
let foo() = begin
        eval(:(issue19774(x::Int) = 2))
        return Compat.invokelatest(issue19774, 0)
    end
    @test foo() == 2
end
cm359() = @__MODULE__
@test Compat.invokelatest(cm359) === @__MODULE__

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

let x = fill!(StringVector(5), 0x61)
    # 0.7
    @test pointer(x) == pointer(String(x))
end

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
@test read(`$(Base.julia_cmd()) --startup-file=no -e "println(:aaaa)"`, String) == "aaaa\n"

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
module Test24714
    using Compat
    using Compat.Test
    using Compat.IterativeEigensolvers
    @test isdefined(@__MODULE__, :IterativeEigensolvers)
    @test isdefined(@__MODULE__, :eigs)
    @test isdefined(@__MODULE__, :svds)
end

# 0.7
module Test24648
    using Compat
    using Compat.Test
    using Compat.SuiteSparse
    @test isdefined(@__MODULE__, :SuiteSparse)
end

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
@test Compat.Unicode.textwidth("A") == 1
@test Compat.Unicode.textwidth('A') == 1

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
@test argmin([10,12,9,11]) == 3
@test argmin([10 12; 9 11]) == CartesianIndex(2, 1)
@test argmin(Dict(:z=>10, :y=>12, :x=>9, :w=>11)) == :x

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
    @test isdefined(@__MODULE__, :SerializationState)
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

# 0.7.0-DEV.4062, but dropped in 0.7.0-DEV.4804
@test Compat.trunc(pi, 3, base = 2) == 3.125
@test Compat.floor(pi, 3, base = 2) == 3.125
@test Compat.ceil(pi, 3, base = 2) == 3.25
@test Compat.round(pi, 3, base = 2) == 3.125
@test Compat.signif(pi, 5, base = 10) == 3.1416

# 0.7.0-DEV.4804
@test Compat.trunc(pi, digits = 3, base = 2) == 3.125
@test Compat.floor(pi, digits = 3, base = 2) == 3.125
@test Compat.ceil(pi, digits = 3, base = 2) == 3.25
@test Compat.round(pi, digits = 3, base = 2) == 3.125
@test Compat.round(pi, sigdigits = 5, base = 10) == 3.1416

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
@test Compat.mean([1 2; 3 4]) == 2.5
@test Compat.mean([1 2; 3 4], dims=1) == [2 3]
@test Compat.mean([1 2; 3 4], dims=2) == hcat([1.5; 3.5])
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
    @test Compat.varm([1 2; 3 4], -1) == 18
    @test Compat.varm([1 2; 3 4], [-1 -2], dims=1) == [20 52]
    @test Compat.varm([1 2; 3 4], [-1, -2], dims=2) == hcat([13, 61])
    @test Compat.var([1 2; 3 4]) == 5/3
    @test Compat.var([1 2; 3 4], dims=1) == [2 2]
    @test Compat.var([1 2; 3 4], dims=2) == hcat([0.5, 0.5])
    @test Compat.var([1 2; 3 4], corrected=false) == 1.25
    @test Compat.var([1 2; 3 4], corrected=false, dims=1) == [1 1]
    @test Compat.var([1 2; 3 4], corrected=false, dims=2) == hcat([0.25, 0.25])
    @test Compat.std([1 2; 3 4]) == sqrt(5/3)
    @test Compat.std([1 2; 3 4], dims=1) == [sqrt(2) sqrt(2)]
    @test Compat.std([1 2; 3 4], dims=2) == hcat([sqrt(0.5), sqrt(0.5)])
    @test Compat.std([1 2; 3 4], corrected=false) == sqrt(1.25)
    @test Compat.std([1 2; 3 4], corrected=false, dims=1) == [sqrt(1) sqrt(1)]
    @test Compat.std([1 2; 3 4], corrected=false, dims=2) == hcat([sqrt(0.25), sqrt(0.25)])
    @test Compat.cov([1 2; 3 4]) == [2 2; 2 2]
    @test Compat.cov([1 2; 3 4], dims=1) == [2 2; 2 2]
    @test Compat.cov([1 2; 3 4], dims=2) == [0.5 0.5; 0.5 0.5]
    @test Compat.cov([1 2; 3 4], [4; 5]) == hcat([1; 1])
    @test Compat.cov([1 2; 3 4], [4; 5], dims=1) == hcat([1; 1])
    @test Compat.cov([1 2; 3 4], [4; 5], dims=2) == hcat([0.5; 0.5])
    @test Compat.cov([1 2; 3 4], [4; 5], corrected=false) == hcat([0.5; 0.5])
    @test Compat.cov([1 2; 3 4], [4; 5], corrected=false, dims=1) == hcat([0.5; 0.5])
    @test Compat.cov([1 2; 3 4], [4; 5], corrected=false, dims=2) == hcat([0.25; 0.25])
    @test Compat.cor([1 2; 3 4]) ≈ [1 1; 1 1]
    @test Compat.cor([1 2; 3 4], dims=1) ≈ [1 1; 1 1]
    @test Compat.cor([1 2; 3 4], dims=2) ≈ [1 1; 1 1]
    @test Compat.cor([1 2; 3 4], [4; 5]) ≈ [1; 1]
    @test Compat.cor([1 2; 3 4], [4; 5], dims=1) ≈ [1; 1]
    @test Compat.cor([1 2; 3 4], [4; 5], dims=2) ≈ [1; 1]
end
@test Compat.median([1 2; 3 4]) == 2.5
@test Compat.median([1 2; 3 4], dims=1) == [2 3]
@test Compat.median([1 2; 3 4], dims=2) == hcat([1.5; 3.5])
@test Compat.mapreduce(string, *, [1 2; 3 4]) == "1324"
Issue26488 && @test Compat.mapreduce(string, *, [1 2; 3 4], dims=1) == ["13" "24"]
Issue26488 && @test Compat.mapreduce(string, *, [1 2; 3 4], dims=2) == hcat(["12", "34"])
@test Compat.mapreduce(string, *, "z", [1 2; 3 4]) == "z1324"
@test Compat.mapreduce(string, *, "z", [1 2; 3 4], dims=1) == ["z13" "z24"]
@test Compat.mapreduce(string, *, "z", [1 2; 3 4], dims=2) == hcat(["z12", "z34"])
@test Compat.reduce(*, [1 2; 3 4]) == 24
@test Compat.reduce(*, [1 2; 3 4], dims=1) == [3 8]
@test Compat.reduce(*, [1 2; 3 4], dims=2) == hcat([2, 12])
@test Compat.reduce(*, 10, [1 2; 3 4]) == 240
@test Compat.reduce(*, 10, [1 2; 3 4], dims=1) == [30 80]
@test Compat.reduce(*, 10, [1 2; 3 4], dims=2) == hcat([20, 120])
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
@test squeeze([1 2], dims=1) == [1, 2]
@test_throws ArgumentError squeeze([1 2], dims=2)
@test_throws ArgumentError squeeze(hcat([1, 2]), dims=1)
@test squeeze(hcat([1, 2]), dims=2) == [1, 2]
@test_throws Exception squeeze([1,2])

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

# 0.7.0-DEV.5278
@test something(nothing, 1) === 1
@test something(Some(2)) === 2
@test something(Some(2), 1) === 2
@test something(nothing, Some(1)) === 1

# 0.7.0-DEV.5171
let sep = Compat.Sys.iswindows() ? ';' : ':'
    withenv("PATH" => string(Compat.Sys.BINDIR, sep, get(ENV, "PATH", ""))) do
        jl = joinpath(Compat.Sys.BINDIR, "julia") * (Compat.Sys.iswindows() ? ".exe" : "")
        @test Compat.Sys.which("julia") == realpath(jl)
        @test Compat.Sys.isexecutable(jl)
        @test Compat.Sys.which("reallyseriouslynotathingyoushouldhave") === nothing
    end
end

nothing
