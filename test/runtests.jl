using Compat
using Base.Test

v = 1

d = Dict{Int,Int}()
d[1] = 1
@test Compat.@Dict(1 => 1) == d
@test Compat.@Dict(1 => v) == d

@test typeof(@compat(Dict(1 => 1))) == Dict{Int,Int}
@test @compat(Dict(1 => 1)) == d
@test @compat(Dict(1 => v)) == d

ad = Dict{Any,Any}()
ad[1] = 1
@test Compat.@AnyDict(1 => 1) == ad
@test Compat.@AnyDict(1 => v) == ad

@test typeof(@compat(Dict{Any,Any}(1 => 1))) == Dict{Any,Any}
@test @compat(Dict{Any,Any}(1 => 1)) == ad
@test @compat(Dict{Any,Any}(1 => v)) == ad

td = Dict{Int,Float64}()
td[1] = 1.0

@test typeof(@compat(Dict{Int,Float64}(1 => 1))) == Dict{Int,Float64}
@test @compat(Dict{Int,Float64}(1 => 1)) == td
@test @compat(Dict{Int,Float64}(1 => v)) == td

@test @compat(Dict()) == Dict()
@test @compat(Dict{Any,Any}()) == Dict{Any,Any}()
if VERSION >= v"0.3.0-"
	@test @compat(Dict([(1, 1)])) == d
end

d2 = Dict{Symbol,Dict{Symbol,Int}}()
d2[:a] = Dict{Symbol,Int}()
d2[:a][:b] = 1
@test @compat(Dict(:a => Dict(:b => 1))) == d2

d = Dict(zip([1, 2], [3, 4]))
@test d == @compat Dict(1=>3, 2=>4)

@compat function f()
	a = :a
	b = Dict(:b => 1)
	Dict(a => b)
end
@test f() == d2

ns = length(d.slots)
@test length(sizehint!(d, ns + 1).slots) > ns

@test @compat split("a,b,,c", ',', limit=2) == ["a", "b,,c"]
@test @compat split("a,b,,c", ',', limit=2,keep=true) == ["a", "b,,c"]
@test @compat split("a,b,,c", ',', keep=false) == ["a", "b", "c"]
@test @compat split("a,b,,c", ',', keep=true) == ["a", "b", "", "c"]

@test @compat rsplit("a,b,,c", ',', limit=2) == ["a,b,", "c"]
@test @compat rsplit("a,b,,c", ',', limit=2,keep=true) == ["a,b,", "c"]
@test @compat rsplit("a,b,,c", ',', keep=false) == ["a", "b", "c"]
@test @compat rsplit("a,b,,c", ',', keep=true) == ["a", "b", "", "c"]

if VERSION < v"0.4.0-dev+1387"
    @test isdefined(Main, :AbstractString)
end

@test round(Int, 3//4) == 1
for x in [(round, iround), (ceil, iceil), (floor, ifloor), (trunc, itrunc)]
    for v = Any[1, 1.1, [1,1], [1.1, 1.1], [1 1], [1.1 1.1]]
        @test x[1](Int,v) == x[2](v)
    end
end

@test IPv4("1.2.3.4") == ip"1.2.3.4"
@test IPv6("2001:1:2:3::1") == ip"2001:1:2:3::1"
@test isless(ip"1.2.3.4", ip"1.2.3.5")

@test startswith("abcdef","abc") == true
@test startswith("abcdef","def") == false

@test size(bitrand(3, 4)) == (3, 4)
@test size(bitrand((3, 4))) == (3, 4)
@test size(bitrand(MersenneTwister(), 3, 4)) == (3, 4)
@test size(bitrand(MersenneTwister(), (3, 4))) == (3, 4)
@test rand(Bool) in [false, true]

module CartesianTest
	using Base.Cartesian, Compat
	@ngenerate N NTuple{N,Int} function f(X::NTuple{N,Int}...)
		@ncall N tuple X
	end
end

@test CartesianTest.f(1) == (1,)
@test CartesianTest.f(1,2) == (1,2)
@test CartesianTest.f(1,2,3) == (1,2,3)
@test CartesianTest.f(1,2,3,4) == (1,2,3,4)
@test CartesianTest.f(1,2,3,4,5) == (1,2,3,4,5)

@test readall(pipe(`echo hello`, `sort`)) == "hello\n"
@test success(pipe(`true`, `true`))

let convert_funcs_and_types =
    ((integer, :Integer), (signed, :Signed), (unsigned, :Unsigned),
     (int, :Int), (int8, :Int8), (int16, :Int16), (int32, :Int32),
     (int64, :Int64), (int128, :Int128), (uint, :UInt),
     (uint8, :UInt8), (uint16, :UInt16), (uint32, :UInt32),
     (uint64, :UInt64), (uint128, :UInt128),
     (float16, :Float16), (float32, :Float32), (float64, :Float64),
     (complex32,:Complex32), (complex64,:Complex64),(complex128,:Complex128),
     (char,:Char))

    for (df,t) in convert_funcs_and_types
        x = @compat UInt8(10)
        r1 = eval(:(@compat($t($x))))
        ty = eval(t)
        if ty.abstract
            @test issubtype(typeof(r1),ty)
        else
            @test typeof(r1) === ty
        end
        if VERSION <  v"0.4.0-dev+3732"
            r2 = df(x)
            @test r1 === r2
            if t === :Signed || t === :Complex32
                continue
            end
            x = fill(x, 10)
            r1 = eval(:(@compat map($t, $x)))
            r2 = df(x)
            @test r1 == r2
            @test typeof(r1) === typeof(r2)
        end
    end
    @test (@compat Bool(1))
    @test !(@compat Bool(0))

    # issue #54
    c = @compat Complex128(1)
    @test c.re == 1
    @test c.im == 0

    c = @compat Complex128(1,1)
    @test c.re == 1
    @test c.im == 1
end

type Test3609
    a
    b
end

if VERSION < v"0.4.0-dev+3609"
    let v = Test3609(1,2)
        @test fieldnames(Test3609) == fieldnames(v) == Symbol[:a, :b]
    end
end

@test parse(Int8, '9') == convert(Int8, 9)
@test parse(Int, 'a', 16) == 10
@test parse(Int, "200") == 200
@test parse(Int16, "1101", 2) == convert(Int16, 13)
@test parse(Float64, "222") == 222.0
@test parse(Float32, "1.1") == convert(Float32, 1.1)

# Make sure exports from Libc and Libdl are defined
for x in [:strftime,:systemsleep,:getpid,:FILE,:malloc,:MS_SYNC,:munmap,:flush_cstdio,:realloc,:strptime,:Libc,:errno,:msync,:TmStruct,:calloc,:MS_INVALIDATE,:MS_ASYNC,:time,:strerror,:gethostname,:free,:mmap]
    Libc.(x)
end
for x in [:RTLD_LOCAL,:RTLD_GLOBAL,:find_library,:dlsym,:RTLD_LAZY,:RTLD_NODELETE,:DL_LOAD_PATH,:RTLD_NOW,:Libdl,:dlext,:dlsym_e,:RTLD_FIRST,:dlopen,:dllist,:dlpath,:RTLD_NOLOAD,:dlclose,:dlopen_e,:RTLD_DEEPBIND]
    Libdl.(x)
end

# Test unsafe_convert
type A; end
x = "abc"
@test bytestring(Compat.unsafe_convert(Ptr{UInt8}, x)) == x
Compat.unsafe_convert(::Ptr{A}, x) = x
@test Compat.unsafe_convert(pointer([A()]), 1) == 1
