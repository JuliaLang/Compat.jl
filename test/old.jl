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
