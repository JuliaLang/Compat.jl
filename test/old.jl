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

# tests of removed functionality (i.e. justs tests Base)

# 25959
@test all(x -> isa(x, IO), (devnull, stdin, stdout, stderr))
@static if !isdefined(Base, :devnull)
    @test stdin === STDIN
    @test stdout === STDOUT
    @test stderr === STDERR
end
