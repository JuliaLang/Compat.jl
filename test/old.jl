# tests of functionality to be deprecated

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

# julia#20006
abstract type AbstractFoo20006 end
struct ConcreteFoo20006{T<:Int} <: AbstractFoo20006 end
struct ConcreteFoo20006N{T<:Int,N} <: AbstractFoo20006 end
@compat ConcreteFoo200061{T<:Int} = ConcreteFoo20006N{T,1}
@test Compat.TypeUtils.isabstract(AbstractFoo20006)
@test !Compat.TypeUtils.isabstract(ConcreteFoo20006)
@test !Compat.TypeUtils.isabstract(ConcreteFoo20006N)
@test !Compat.TypeUtils.isabstract(ConcreteFoo200061)
@test !Compat.TypeUtils.isabstract(StridedArray)
@test Compat.TypeUtils.parameter_upper_bound(ConcreteFoo20006, 1) == Int
@test isa(Compat.TypeUtils.typename(Array), Core.TypeName)

# invokelatest with keywords
pr22646(x; y=0) = 1
let foo() = begin
        eval(:(pr22646(x::Int; y=0) = 2))
        return Compat.invokelatest(pr22646, 0, y=1)
    end
    @test foo() == 2
end

for os in [:apple, :bsd, :linux, :unix, :windows]
    from_base = if VERSION >= v"0.7.0-DEV.914"
        Expr(:., Expr(:., :Base, Base.Meta.quot(:Sys)), Base.Meta.quot(Symbol("is", os)))
    else
        Expr(:., :Base, Base.Meta.quot(Symbol("is_", os)))
    end
    @eval @test Compat.Sys.$(Symbol("is", os))() == $from_base()
end

# 0.7.0-DEV.3073
@test Compat.Sys.BINDIR == Sys.BINDIR

# 0.7.0-DEV.5171
let sep = Compat.Sys.iswindows() ? ';' : ':'
    withenv("PATH" => string(Compat.Sys.BINDIR, sep, get(ENV, "PATH", ""))) do
        jl = joinpath(Compat.Sys.BINDIR, "julia") * (Compat.Sys.iswindows() ? ".exe" : "")
        @test Compat.Sys.which("julia") == realpath(jl)
        @test Compat.Sys.isexecutable(jl)
        @test Compat.Sys.which("reallyseriouslynotathingyoushouldhave") === nothing
    end
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

# 0.7.0-DEV.3382
module TestLibdl
    using Compat
    using Compat.Libdl
    using Compat.Test
    @test isdefined(@__MODULE__, :Libdl)
end

# 0.7.0-DEV.3216
@test Compat.AbstractDateTime === (isdefined(Compat.Dates, :AbstractDateTime) ? Compat.Dates.AbstractDateTime : Compat.Dates.TimeType)
@test Compat.AbstractDateTime <: Compat.Dates.TimeType
@test Compat.Dates.DateTime <: Compat.AbstractDateTime

# 0.7
module Test25056
    using Compat
    using Compat.Test
    using Compat.Printf
    @test isdefined(@__MODULE__, :Printf)
    @test isdefined(@__MODULE__, Symbol("@printf"))
    @test isdefined(@__MODULE__, Symbol("@sprintf"))
end

# 0.7.0-DEV.3449
let A = [2.0 1.0; 1.0 3.0], b = [2.0, 3.0]
    @test diag(A) == b
end

# 0.7.0-DEV.3406
using Compat.Random
@test rand(MersenneTwister(1234)) == 0.5908446386657102

# 0.7.0-beta2.171
Random.seed!(1)
rng = MersenneTwister(0)
Random.seed!(rng, 1)
@test rand(rng) ≈ 0.23603334566204692
@test 0 < rand(Random.GLOBAL_RNG, Random.RangeGenerator(1:3)) < 4

# 0.7.0-DEV.3589
import Compat.Markdown
@test isa(Markdown.parse("foo"), Markdown.MD)

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

# 0.7.0-DEV.2338
module Test24361
    using Compat
    using Compat.Test
    @test String(Compat.Base64.base64decode("SGVsbG8h")) == "Hello!"
end

# julia#26365
@test Compat.tr([1 2; 3 5]) == 6

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

# 0.7.0-DEV.3017
@test Compat.notnothing(1) == 1
@test_throws ArgumentError Compat.notnothing(nothing)

# 0.7.0-DEV.3309
let v = [1, 2, 3]
    @test Compat.IteratorSize(v) isa Base.HasShape
    @test Compat.IteratorEltype(v) == Base.HasEltype()
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


# tests of removed functionality (i.e. justs tests Base)

# 25959
@test all(x -> isa(x, IO), (devnull, stdin, stdout, stderr))
@static if !isdefined(Base, :devnull)
    @test stdin === STDIN
    @test stdout === STDOUT
    @test stderr === STDERR
end

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

# PR 22064
module Test22064
using Compat
using Compat.Test
@test (@__MODULE__) === Test22064
end

@test isabstracttype(AbstractFoo20006)
@test !isabstracttype(ConcreteFoo20006)
@test !isabstracttype(ConcreteFoo20006N)
@test !isabstracttype(ConcreteFoo200061)
@test !isabstracttype(StridedArray)
# 0.7
@test isconcretetype(Int)

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

# PR 22629
@test logdet(0.5) == log(det(0.5))

# 0.7.0-DEV.3155
let coolvec = [1,2,3]
    @test pushfirst!(coolvec, 0) == [0,1,2,3]
    @test popfirst!(coolvec) == 0
end

# PR 22350
struct TestType
    a::Int
    b
end
@test fieldcount(TestType) == 2
@test fieldcount(Int) == 0

# 0.7
@test read(IOBuffer("aaaa"), String) == "aaaa"
@test occursin("read(@__FILE__, String)", read(@__FILE__, String))
let cmd = `$(Base.julia_cmd()) --startup-file=no -e "println(:aaaa)"`
    @test read(cmd, String) == "aaaa\n"
    @test read(pipeline(cmd, stderr=devnull), String) == "aaaa\n"
end

# PR 20005
@test_throws InexactError throw(InexactError(:func, Int, 3.2))

# PR 22751
@test_throws DomainError throw(DomainError(-2))
@test_throws DomainError throw(DomainError(-2, "negative"))

# PR 22761
@test_throws OverflowError throw(OverflowError("overflow"))

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
@test partialsort([3,6,30,1,9], 2, rev=true) == 9
@test partialsort([3,6,30,1,9], 2, by=x->1/x) == 9
@test partialsortperm([3,6,30,1,9], 2, rev=true) == 5
@test partialsortperm([3,6,30,1,9], 2, by=x->1/x) == 5

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

# 0.7
@test isa(1:2, AbstractRange)

# 0.7
@test isa(Base.rtoldefault(1.0, 2.0, 0), Float64)
@test isa(Base.rtoldefault(Float64, 2.0, 0), Float64)
@test isa(Base.rtoldefault(1.0, Float64, 0), Float64)
@test isa(Base.rtoldefault(Float64, Float64, 0), Float64)
@test Base.rtoldefault(Float64, Float64, 1.0) === 0.0

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

# 0.7.0-DEV.4639
@test occursin(r"World", "Hello, World!")
@test occursin(r"World", "Hello, World!", offset = 4)
@test occursin("World", "Hello, World!")
# 0.7.0-DEV.912
@test occursin('W', "Hello, World!")

# 0.7
@test 'a'*"b" == "a"*'b' == 'a'*'b' == "ab"

# 0.7
@test 1 in BitSet(1:10)

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

# 0.7.0-DEV.1472
@test get(IOContext(IOBuffer(), :arg1=>true, :arg2=>true, :arg3=>true), :arg3, false)
@test get(IOContext(IOBuffer(), :arg1=>true, :arg2=>true), :arg2, false)

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

# 0.7.0-DEV.1499
let key = "TEST_23412"
    @test !haskey(ENV, key)
    @test get(() -> "default", ENV, key) == "default"
end

# 0.7.0-DEV.2919
@test ComplexF16 === Complex{Float16}
@test ComplexF32 === Complex{Float32}
@test ComplexF64 === Complex{Float64}

# 0.7.0-DEV.1930
@test textwidth("A") == 1
@test textwidth('A') == 1

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

# 0.7.0-DEV.3173
@test invpermute!(permute!([1, 2], 2:-1:1), 2:-1:1) == [1, 2]

# 0.7.0-DEV.3172
@test replace("abcb", "b"=>"c") == "accc"
@test replace("abcb", "b"=>"c", count=1) == "accb"

# 0.7.0-DEV.3057
let A = [0, 0, 0], B = [1, 2, 3]
    @test copyto!(A, B) === A == B
end
let A = [0, 0, 0], B = [1, 2, 3]
    @test unsafe_copyto!(A, 2, B, 1, 1) === A == [0, 1, 0]
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

# Issue #523
@test length(Compat.CartesianIndices((1:1,))) == 1
@test length(Compat.CartesianIndices((1:2,))) == 2
@test length(Compat.CartesianIndices((1:2, -1:1))) == 6

# 0.7.0-DEV.3415
@test findall(x -> x==1, [1, 2, 3, 2, 1]) == [1, 5]

# 0.7.0-DEV.3516
@test argmax([10,12,9,11]) == 2
@test argmax([10 12; 9 11]) == CartesianIndex(1, 2)
@test argmax(Dict(:z=>10, :y=>12, :x=>9, :w=>11)) == :y
@test argmax((-5, 6, 10)) == 3
@test argmin([10,12,9,11]) == 3
@test argmin([10 12; 9 11]) == CartesianIndex(2, 1)
@test argmin(Dict(:z=>10, :y=>12, :x=>9, :w=>11)) == :x
@test argmin((1.0, -3, 0.f0)) == 2

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
