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
