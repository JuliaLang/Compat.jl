module Compat

using Base.Meta

if VERSION >= v"0.4.0-dev+3184"
    include("ngenerate.jl")
    using .CompatCartesian
    export @ngenerate, @nsplat
end

if VERSION < v"0.4.0-dev+1624"
    eachindex(A::AbstractArray) = 1:length(A)
    export eachindex
end

if VERSION < v"0.4.0-dev+1419"
    export UInt, UInt8, UInt16, UInt32, UInt64, UInt128
    const UInt = Uint
    const UInt8 = Uint8
    const UInt16 = Uint16
    const UInt32 = Uint32
    const UInt64 = Uint64
    const UInt128 = Uint128
end

if VERSION < v"0.4.0-dev+1387"
    typealias AbstractString String
    export AbstractString
end

if VERSION < v"0.4.0-dev+3324"
    typealias AssertionError ErrorException
    export AssertionError
end

if VERSION < v"0.4.0-dev+412"
    eval(Base, :(const IPAddr = IpAddr))
end

if VERSION < v"0.4.0-dev+2197"
    Base.IPv4(ipstr::AbstractString) = Base.parseipv4(ipstr)
    Base.IPv6(ipstr::AbstractString) = Base.parseipv6(ipstr)
end

if VERSION < v"0.4.0-dev+2200"
    function Base.isless{T<:Base.IPAddr}(a::T, b::T)
        return isless(a.host, b.host)
    end
end

if VERSION < v"0.4.0-dev+980"
    macro Dict(pairs...)
        esc(Expr(:dict, pairs...))
    end
    macro AnyDict(pairs...)
        esc(Expr(:typed_dict, :(Any=>Any), pairs...))
    end

    Base.Dict(kv) = dict_with_eltype(kv, eltype(kv))
    dict_with_eltype{K,V}(kv, ::Type{(K,V)}) = Dict{K,V}(kv)
    dict_with_eltype(kv, t) = Dict{Any,Any}(kv)
else
    macro Dict(pairs...)
        esc(Expr(:call, :Dict, pairs...))
    end
    macro AnyDict(pairs...)
        esc(Expr(:call, :(Base.AnyDict), pairs...))
    end
end

import Base: round, ceil, floor, trunc
if VERSION < v"0.4.0-dev+1827"
    for (fnew,fold) in ((:round, :iround), (:ceil, :iceil), (:floor, :ifloor), (:trunc, :itrunc))
        @eval begin
            ($fnew){T<:Integer}(::Type{T}, x::Integer) = convert(T, x)  # ambiguity resolution with digits/base version, not all old methods defined
            ($fnew){T<:Integer}(::Type{T}, x::Real) = ($fold)(T, x)
            ($fnew){T<:Integer}(::Type{T}, x::Rational) = convert(T, ($fold)(x)) # no e.g. iround(::Type{T}, x::Rational) is defined in 0.3
            function ($fnew){T,R}(::Type{T}, x::AbstractArray{R,1})
                [ ($fnew)(T, x[i])::T for i = 1:length(x) ]
            end
            function ($fnew){T,R}(::Type{T}, x::AbstractArray{R,2})
                [ ($fnew)(T, x[i,j])::T for i = 1:size(x,1), j = 1:size(x,2) ]
            end
            function ($fnew){T}(::Type{T}, x::AbstractArray)
                reshape([ ($fnew)(T, x[i])::T for i = 1:length(x) ], size(x))
            end
        end
    end
end

if VERSION < v"0.4.0-dev+1884"
    randexp(rng::MersenneTwister) = Base.Random.randmtzig_exprnd(rng)
    randexp() = Base.Random.randmtzig_exprnd()
    export randexp
end

if VERSION < v"0.4.0-dev+2014"
    sizehint! = Base.sizehint
    export sizehint!
end

if VERSION < v"0.4.0-dev+3413"
    # based on pipe in base/process.jl:
    import Base: AbstractCmd, Redirectable
    function pipe(cmd::AbstractCmd; stdin=nothing, stdout=nothing, stderr=nothing, append::Bool=false)
        if append && stdout === nothing && stderr === nothing
            error("append set to true, but no output redirections specified")
        end
        if stdin !== nothing
            cmd = stdin |> cmd
        end
        if stdout !== nothing
            cmd = append ? cmd >> stdout : cmd |> stdout
        end
        if stderr !== nothing
            cmd = append ? cmd .>> stderr : cmd .> stderr
        end
        return cmd
    end
    pipe(cmd::AbstractCmd, dest) = pipe(cmd, stdout=dest)
    pipe(src::Union(Redirectable,AbstractString), cmd::AbstractCmd) = pipe(cmd, stdin=src)
    pipe(a, b, c, d...) = pipe(pipe(a,b), c, d...)
    export pipe
end

function rewrite_dict(ex)
    length(ex.args) == 1 && return ex

    f = ex.args[1]
    if isexpr(f, :curly)
        newex = Expr(:typed_dict, :($(f.args[2])=>$(f.args[3])))
    else
        newex = Expr(:dict)
    end

    for i = 2:length(ex.args)
        pair = ex.args[i]
        !isexpr(pair, :(=>)) && return ex
        push!(newex.args, pair)
    end
    newex
end

# rewrite Julia 0.4-style split or rsplit (str, splitter; kws...)
# into 0.2/0.3-style positional arguments
function rewrite_split(ex, f)
    limit = nothing
    keep = nothing
    for i in 4:length(ex.args)
        if isexpr(ex.args[i], :kw)
            kw = ex.args[i].args
            if kw[1] == :limit
                limit = kw[2]
            elseif kw[1] == :keep
                keep = kw[2]
            end
        end
    end
    if limit == nothing
        if keep == nothing
            return Expr(:call, f, ex.args[2], ex.args[3])
        else
            return Expr(:call, f, ex.args[2], ex.args[3], keep)
        end
    else
        if keep == nothing
            return Expr(:call, f, ex.args[2], ex.args[3], limit)
        else
            return Expr(:call, f, ex.args[2], ex.args[3], limit, keep)
        end
    end
end

if VERSION < v"0.4.0-dev+707"
    macro inline(ex)
        esc(ex)
    end
end

if VERSION < v"0.4.0-dev+2056"
    macro noinline(ex)
        esc(ex)
    end
end

if VERSION < v"0.4.0-dev+2440"
    bitrand(r::AbstractRNG, dims::Dims)   = rand!(r, BitArray(dims))
    bitrand(r::AbstractRNG, dims::Int...) = rand!(r, BitArray(dims))
    bitrand(dims::Dims)   = rand!(BitArray(dims))
    bitrand(dims::Int...) = rand!(BitArray(dims))
    export bitrand
    Base.rand(::Type{Bool}) = randbool()
end

if VERSION < v"0.4.0-dev+2485"
    startswith = Base.beginswith
    export startswith
end

if VERSION < v"0.4.0-dev+3609"
    fieldnames(x::DataType) = collect(x.names)
    function fieldnames(v)
        t = typeof(v)
        if !isa(t, DataType)
            throw(ArgumentError("cannot call fieldnames() on a non-composite type"))
        end
        return fieldnames(t)
    end
    export fieldnames
end

if VERSION < v"0.4.0-dev+3874"
    Base.parse{T<:Integer}(::Type{T}, c::Char) = parseint(T, c)
    Base.parse{T<:Integer}(::Type{T}, c::Char, base::Integer) = parseint(T, c, base)
    Base.parse{T<:Integer}(::Type{T}, s::AbstractString) = parseint(T, s)
    Base.parse{T<:Integer}(::Type{T}, s::AbstractString, base::Integer) = parseint(T, s, base)
    Base.parse{T<:Union(Float32,Float64)}(::Type{T}, s::AbstractString) = parsefloat(T, s)
end

if VERSION < v"0.4.0-dev+4539"
    Base.parse(::Type{BigFloat}, s::AbstractString) = BigFloat(s)
end

if VERSION < v"0.4.0-dev+3710"
    const unsafe_convert = Base.convert
else
    import Base.unsafe_convert
end

if VERSION < v"0.4.0-dev+3732"
    const calltypes = Dict{Symbol,Symbol}()
    for (k,v) in [(:Integer,:integer),
                   (:Signed,:signed),
                   (:Unsigned,:unsigned),
                   (:Int,:int),
                   (:Int8,:int8),
                   (:Int16,:int16),
                   (:Int32,:int32),
                   (:Int64,:int64),
                   (:Int128,:int128),
                   (:UInt,:uint),
                   (:UInt8,:uint8),
                   (:UInt16,:uint16),
                   (:UInt32,:uint32),
                   (:UInt64,:uint64),
                   (:UInt128,:uint128),
                   (:Float16,:float16),
                   (:Float32,:float32),
                   (:Float64,:float64),
                   (:Complex32,:complex32),
                   (:Complex64,:complex64),
                   (:Complex128,:complex128),
                   (:Char,:char),
                   (:Bool,:bool)]
        calltypes[k] = v
    end
end

function _compat(ex::Expr)
    if ex.head == :call
        f = ex.args[1]
        if VERSION < v"0.4.0-dev+980" && (f == :Dict || (isexpr(f, :curly) && length(f.args) == 3 && f.args[1] == :Dict))
            ex = rewrite_dict(ex)
        elseif VERSION < v"0.4.0-dev+129" && (f == :split || f == :rsplit) && length(ex.args) >= 4 && isexpr(ex.args[4], :kw)
            ex = rewrite_split(ex, f)
        elseif VERSION < v"0.4.0-dev+3732" && haskey(calltypes, f) && length(ex.args) > 1
            T = ex.args[1]
            if T in (:Complex32, :Complex64, :Complex128)
                ex = Expr(:call, calltypes[T], ex.args[2:end]...)
            else
                ex = Expr(:(::), Expr(:call, :convert, T, ex.args[2:end]...), T)
            end
        elseif VERSION < v"0.4.0-dev+3732" && (f == :map && haskey(calltypes, ex.args[2]) && length(ex.args) > 2)
            ex = Expr(:call, calltypes[ex.args[2]], ex.args[3:end]...)
        elseif VERSION < v"0.4.0-dev+1419" && isexpr(f, :curly) && f.args[1] == :Ptr && length(ex.args) == 2 && ex.args[2] == 0
            ex = Expr(:call, :zero, f)
        elseif VERSION < v"0.4.0-dev+4356" && f == :chol
            s = ex.args[3]
            if isexpr(s, :curly) && s.args[1] == :Val
                ex = Expr(:call, :chol, ex.args[2], s.args[2])
            end
        elseif VERSION < v"0.4.0-dev+4739" && isexpr(f, :curly) && (f.args[1] == :Vector || f.args[1] == :Array)
            if f.args[1] == :Vector && length(ex.args) == 1
                ex = Expr(:call, :Array, f.args[2], 0)
            else
                ex = Expr(:call, :Array, f.args[2], ex.args[2:end]...)
            end
        end
    elseif ex.head == :curly
        f = ex.args[1]
        if VERSION < v"0.4.0-dev+4319" && f == :Tuple
            args = ex.args[2:end]
            has_ellipsis = any(args) do arg
                isa(arg, Expr) && (arg.head == :...)
            end
            ex = if has_ellipsis
                Expr(:call, TopNode(:tuple), args...)
            else
                Expr(:tuple, args...)
            end
        end
    elseif ex.head == :macrocall
        f = ex.args[1]
        if f == symbol("@generated") && VERSION < v"0.4.0-dev+4387"
            f = ex.args[2]
            if isexpr(f, :function)
                ex = Expr(:stagedfunction, f.args...)
            end
        elseif f == symbol("@doc") && VERSION < v"0.4.0-dev+5873"
            # Replace the quote with a direct macrocall for the special cases
            # `@doc "<doc string>" :(@macro)` and
            # `@doc "<doc string>" :(str_macro"")`
            # PR JuliaLang/julia#12000
            if (length(ex.args) == 3 && isexpr(ex.args[3], :quote) &&
                length(ex.args[3].args) == 1 &&
                isexpr(ex.args[3].args[1], :macrocall))
                ex = Expr(:macrocall, f, ex.args[2], ex.args[3].args[1])
            end
        end
    elseif VERSION < v"0.4.0-dev+5322" && ex.head == :(::) && isa(ex.args[end], Symbol)
        # Replace Base.Timer with Compat.Timer2 in type declarations
        if ex.args[end] == :Timer || ex.args[end] == :(Base.Timer)
            ex.args[end] = :(Compat.Timer2)
        end
    end
    return Expr(ex.head, map(_compat, ex.args)...)
end
_compat(ex) = ex

macro compat(ex)
    esc(_compat(ex))
end

export @compat, @inline, @noinline

if VERSION < v"0.4.0-dev+656"
    include("nullable.jl")
end

if VERSION < v"0.4.0-dev+3864"
    function tryparse(T::Type{Float32}, s)
        r = Array(T,1)
        float32_isvalid(s, r) ? Nullable(r[1]) : Nullable{Float32}()
    end

    function tryparse(T::Type{Float64}, s)
        r = Array(T,1)
        float64_isvalid(s, r) ? Nullable(r[1]) : Nullable{Float64}()
    end

    function tryparse{T<:Integer}(::Type{T}, s)
        local ret
        try
            val = parse(T, s)
            ret = Nullable{T}(val)
        catch
            ret = Nullable{T}()
        end
        return ret
    end
    export tryparse
end

if VERSION < v"0.4.0-dev+3844"
    @eval module Libc
        const FILE = Base.CFILE
        const calloc = Base.c_calloc
        const free = Base.c_free
        const malloc = Base.c_malloc
        const realloc = Base.c_realloc
        using Base: munmap, systemsleep
        export FILE, TmStruct, calloc, errno, flush_cstdio, free,
               gethostname, getpid, malloc, msync, realloc, strerror,
               strftime, strptime, systemsleep, time
    end
    @eval module Libdl
        using Base.Sys: dlext, dllist, dlpath
        export DL_LOAD_PATH, RTLD_DEEPBIND, RTLD_FIRST, RTLD_GLOBAL, RTLD_LAZY,
               RTLD_LOCAL, RTLD_NODELETE, RTLD_NOLOAD, RTLD_NOW, dlclose, dlext,
               dllist, dlopen, dlopen_e, dlpath, dlsym, dlsym_e, find_library
    end
    export Libc, Libdl
else
    import Base: Libc, Libdl
end

if VERSION < v"0.4.0-dev+2418"
    function findprev(A, start)
        for i = start:-1:1
            A[i] != 0 && return i
        end
        0
    end
    findlast(A) = findprev(A, length(A))
    function findprev(A, v, start)
        for i = start:-1:1
            A[i] == v && return i
        end
        0
    end
    findlast(A, v) = findprev(A, v, length(A))
    function findprev(testf::Function, A, start)
        for i = start:-1:1
            testf(A[i]) && return i
        end
        0
    end
    findlast(testf::Function, A) = findprev(testf, A, length(A))
    export findprev, findlast
end

if VERSION < v"0.4.0-dev+4524"
    isdiag(A::AbstractMatrix) = istril(A) && istriu(A)
    isdiag(x::Number) = true
    export isdiag
end

if VERSION < v"0.4.0-dev+4502"
    keytype{K,V}(::Associative{K,V}) = K
    valtype{K,V}(::Associative{K,V}) = V
    export keytype, valtype
end

if VERSION < v"0.4.0-dev+2254"
    immutable Val{T} end
    export Val
end

if VERSION < v"0.4.0-dev+4603"
    # used for C string arguments to ccall
    # (in Julia 0.4, these types also check for embedded NUL chars)
    const Cstring = Ptr{Cchar}
    const Cwstring = Ptr{Cwchar_t}
    export Cstring, Cwstring
end

if VERSION < v"0.4.0-dev+2823"
    fma(x::Number,y::Number,z::Number) = x*y + z
    export fma
end

if VERSION < v"0.4.0-dev+2861"
    muladd(x::Number,y::Number,z::Number) = x*y + z
    export muladd
end

if VERSION < v"0.4.0-dev+4734"
    function is_valid_utf32(str::Union(Vector{Char}, Vector{UInt32}))
        for i=1:length(str)
            @inbounds if !is_valid_char(reinterpret(UInt32, str[i])) ; return false ; end
        end
        return true
    end
    is_valid_utf32(str::UTF32String) = is_valid_utf32(str.data)
    export is_valid_utf32
end

if VERSION < v"0.4.0-dev+4939"
    import Base.isvalid
    isvalid(ch::Char)         = is_valid_char(ch)
    isvalid(str::ASCIIString) = is_valid_ascii(str)
    isvalid(str::UTF8String)  = is_valid_utf8(str)
    isvalid(str::UTF16String) = is_valid_utf16(str)
    isvalid(str::UTF32String) = is_valid_utf32(str)
    isvalid(::Type{Char}, ch)         = is_valid_char(ch)
    isvalid(::Type{ASCIIString}, str) = is_valid_ascii(str)
    isvalid(::Type{UTF8String},  str) = is_valid_utf8(str)
    isvalid(::Type{UTF16String}, str) = is_valid_utf16(str)
    isvalid(::Type{UTF32String}, str) = is_valid_utf32(str)
    export isvalid
end

if VERSION < v"0.4.0-dev+5322"
    include("timer.jl")
end

if VERSION < v"0.4.0-dev+5688"
    typealias Irrational MathConst
    @eval const $(symbol("@irrational")) = getfield(Base, symbol("@math_const"))
    export Irrational
else
    import Base.@irrational
end

if VERSION < v"0.4.0-dev+5305"
    function Base.gc_enable(on::Bool)
        state = ccall(:jl_gc_is_enabled, Cint, ()) != 0
        on ? gc_enable() : gc_disable()
        state
    end
end

end # module
