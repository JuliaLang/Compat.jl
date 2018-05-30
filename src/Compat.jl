__precompile__()

module Compat

# https://github.com/JuliaLang/julia/pull/25935
if VERSION < v"0.7.0-DEV.4442"
    @eval module Sockets
        import Base:
            @ip_str, IPAddr, IPv4, IPv6, UDPSocket, TCPSocket,
            accept, connect, getaddrinfo, getipaddr, getsockname, listen,
            listenany, recv, recvfrom, send, bind

        export
            @ip_str, IPAddr, IPv4, IPv6, UDPSocket, TCPSocket,
            accept, connect, getaddrinfo, getipaddr, getsockname, listen,
            listenany, recv, recvfrom, send, bind
    end
else
    import Sockets
end

include("compatmacro.jl")

@static if !isdefined(Base, :devnull) #25959
    export devnull, stdout, stdin, stderr
    const devnull = DevNull
    for f in (:stdout, :stdin, :stderr)
        F = Symbol(uppercase(string(f)))
        rf = Symbol(string("_redirect_", f))
        @eval begin
            $f = $F
            # overload internal _redirect_std* functions
            # so that they change Compat.std*
            function Base.$rf(stream::IO)
                ret = invoke(Base.$rf, Tuple{Any}, stream)
                global $f = $F
                return ret
            end
        end
    end
    # in __init__ because these can't be saved during precompiling
    function __init__()
        global stdout = STDOUT
        global stdin = STDIN
        global stderr = STDERR
    end
end

@static if !isdefined(Base, Symbol("@nospecialize"))
    # 0.7
    macro nospecialize(arg)
        if isa(arg, Symbol)
            # @nospecialize(arg)
            return :($(esc(arg))::ANY)
        elseif isa(arg, Expr) && arg.head == :(::)
            # @nospecialize(arg::typ)
            # unsupported: needs ::ANY which would change dispatch as determined by ::typ
        elseif isa(arg, Expr) && arg.head == :(=)
            # @nospecialize(arg=val)
            arg, val = arg.args
            if isa(arg, Expr) && arg.head == :(::)
                # @nospecialize(arg::typ=val)
                # unsupported (see above), but generate a kw arg
                arg, typ = arg.args
                return Expr(:kw, :($(esc(arg))::$(esc(typ))), esc(val))
            else
                return Expr(:kw, :($(esc(arg))::ANY), esc(val))
            end
        end
        return esc(arg)
    end
    export @nospecialize
end

if VERSION < v"0.6.0-dev.2043"
    Base.take!(t::Task) = consume(t)
end

# https://github.com/JuliaLang/julia/pull/22064
@static if !isdefined(Base, Symbol("@__MODULE__"))
    # 0.7
    export @__MODULE__
    macro __MODULE__()
        return current_module()
    end
    Base.expand(mod::Module, x::ANY) = eval(mod, :(expand($(QuoteNode(x)))))
    Base.macroexpand(mod::Module, x::ANY) = eval(mod, :(macroexpand($(QuoteNode(x)))))
    Base.include_string(mod::Module, code::String, fname::String) =
        eval(mod, :(include_string($code, $fname)))
    Base.include_string(mod::Module, code::AbstractString, fname::AbstractString="string") =
        eval(mod, :(include_string($code, $fname)))
end

if VERSION < v"0.6.0-dev.2042"
    include_string(@__MODULE__, """
        immutable ExponentialBackOff
            n::Int
            first_delay::Float64
            max_delay::Float64
            factor::Float64
            jitter::Float64

            function ExponentialBackOff(n, first_delay, max_delay, factor, jitter)
                all(x->x>=0, (n, first_delay, max_delay, factor, jitter)) || error("all inputs must be non-negative")
                new(n, first_delay, max_delay, factor, jitter)
            end
        end
    """)

    """
        ExponentialBackOff(; n=1, first_delay=0.05, max_delay=10.0, factor=5.0, jitter=0.1)

    A [`Float64`](@ref) iterator of length `n` whose elements exponentially increase at a
    rate in the interval `factor` * (1 ± `jitter`).  The first element is
    `first_delay` and all elements are clamped to `max_delay`.
    """
    ExponentialBackOff(; n=1, first_delay=0.05, max_delay=10.0, factor=5.0, jitter=0.1) =
        ExponentialBackOff(n, first_delay, max_delay, factor, jitter)
    Base.start(ebo::ExponentialBackOff) = (ebo.n, min(ebo.first_delay, ebo.max_delay))
    function Base.next(ebo::ExponentialBackOff, state)
        next_n = state[1]-1
        curr_delay = state[2]
        next_delay = min(ebo.max_delay, state[2] * ebo.factor * (1.0 - ebo.jitter + (rand() * 2.0 * ebo.jitter)))
        (curr_delay, (next_n, next_delay))
    end
    Base.done(ebo::ExponentialBackOff, state) = state[1]<1
    Base.length(ebo::ExponentialBackOff) = ebo.n

    function retry(f::Function;  delays=ExponentialBackOff(), check=nothing)
        (args...; kwargs...) -> begin
            state = start(delays)
            while true
                try
                    return f(args...; kwargs...)
                catch e
                    done(delays, state) && rethrow(e)
                    if check !== nothing
                        state, retry_or_not = check(state, e)
                        retry_or_not || rethrow(e)
                    end
                end
                (delay, state) = next(delays, state)
                sleep(delay)
            end
        end
    end
else
    import Base.ExponentialBackOff
    import Base.retry
end

import Base: redirect_stdin, redirect_stdout, redirect_stderr
if VERSION < v"0.6.0-dev.374"
    for (F,S) in ((:redirect_stdin, :STDIN), (:redirect_stdout, :STDOUT), (:redirect_stderr, :STDERR))
        @eval function $F(f::Function, stream)
            STDOLD = $S
            $F(stream)
            try f() finally $F(STDOLD) end
        end
    end
end

@static if VERSION < v"0.6.0-dev.528"
    macro __DIR__()
        Base.source_dir()
    end
    export @__DIR__
end

# PR #17302
# Provide a non-deprecated version of `@vectorize_(1|2)arg` macro which defines
# deprecated version of the function so that the depwarns can be fixed without
# breaking users.
# Packages are expected to use this to maintain the old API until all users
# of the deprecated vectorized function have migrated.
# These macros should raise a depwarn when the `0.5` support is dropped from
# `Compat` and be dropped when the support for `0.6` is dropped from `Compat`.
# Modified based on the version copied from 0.6 Base.
if VERSION < v"0.7.0-DEV.1211"
    macro dep_vectorize_1arg(S, f)
        S = esc(S)
        f = esc(f)
        T = esc(:T)
        x = esc(:x)
        AbsArr = esc(:AbstractArray)
        ## Depwarn to be enabled when 0.5 support is dropped.
        # depwarn("Implicit vectorized function is deprecated in favor of compact broadcast syntax.",
        #         Symbol("@dep_vectorize_1arg"))
        :(@deprecate $f{$T<:$S}($x::$AbsArr{$T}) @compat($f.($x)))
    end

    macro dep_vectorize_2arg(S, f)
        S = esc(S)
        f = esc(f)
        T1 = esc(:T1)
        T2 = esc(:T2)
        x = esc(:x)
        y = esc(:y)
        AbsArr = esc(:AbstractArray)
        ## Depwarn to be enabled when 0.5 support is dropped.
        # depwarn("Implicit vectorized function is deprecated in favor of compact broadcast syntax.",
        #         Symbol("@dep_vectorize_2arg"))
        quote
            @deprecate $f{$T1<:$S}($x::$S, $y::$AbsArr{$T1}) @compat($f.($x,$y))
            @deprecate $f{$T1<:$S}($x::$AbsArr{$T1}, $y::$S) @compat($f.($x,$y))
            @deprecate $f{$T1<:$S,$T2<:$S}($x::$AbsArr{$T1}, $y::$AbsArr{$T2}) @compat($f.($x,$y))
        end
    end
else
    macro dep_vectorize_1arg(S, f)
        AbstractArray = GlobalRef(Base, :AbstractArray)
        return esc(:(@deprecate $f(x::$AbstractArray{T}) where {T<:$S} $f.(x)))
    end

    macro dep_vectorize_2arg(S, f)
        AbstractArray = GlobalRef(Base, :AbstractArray)
        return esc(quote
            @deprecate $f(x::$S, y::$AbstractArray{T1}) where {T1<:$S} $f.(x, y)
            @deprecate $f(x::$AbstractArray{T1}, y::$S) where {T1<:$S} $f.(x, y)
            @deprecate $f(x::$AbstractArray{T1}, y::$AbstractArray{T2}) where {T1<:$S, T2<:$S} $f.(x, y)
        end)
    end
end

# broadcast over same length tuples, from julia#16986
@static if VERSION < v"0.6.0-dev.693"
    Base.Broadcast.broadcast{N}(f, t::NTuple{N}, ts::Vararg{NTuple{N}}) = map(f, t, ts...)
end

# julia#18510
if VERSION < v"0.6.0-dev.826"
    _Nullable_field2(x) = !x
else
    _Nullable_field2(x) = x
end

# julia#18484
@static if VERSION < v"0.6.0-dev.848"
    unsafe_get(x::Nullable) = x.value
    unsafe_get(x) = x
    export unsafe_get
    Base.isnull(x) = false
end

# julia#18977
@static if !isdefined(Base, :xor)
    # 0.6
    const xor = $
    const ⊻ = xor
    export xor, ⊻
end

# julia#19246
@static if !isdefined(Base, :numerator)
    # 0.6
    const numerator = num
    const denominator = den
    export numerator, denominator
end

# julia #19950
@static if !isdefined(Base, :iszero)
    # 0.6
    iszero(x) = x == zero(x)
    iszero(x::Number) = x == 0
    iszero(x::AbstractArray) = all(iszero, x)
    export iszero
end

# julia　#20407
@static if !isdefined(Base, :(>:))
    # 0.6
    const >: = let
        _issupertype(a::ANY, b::ANY) = issubtype(b, a)
    end
    export >:
end

# julia#19088
if VERSION < v"0.6.0-dev.1256"
    Base.take!(io::Base.AbstractIOBuffer) = takebuf_array(io)
end

if VERSION < v"0.6.0-dev.1632"
    # To work around unsupported syntax on Julia 0.4
    include_string("export .&, .|")
    include_string(".&(xs...) = broadcast(&, xs...)")
    include_string(".|(xs...) = broadcast(|, xs...)")
end

@static if VERSION < v"0.6.0-dev.2093" # Compat.isapprox to allow for NaNs
    using Base.rtoldefault
    function isapprox(x::Number, y::Number; rtol::Real=rtoldefault(x,y), atol::Real=0, nans::Bool=false)
        x == y || (isfinite(x) && isfinite(y) && abs(x-y) <= atol + rtol*max(abs(x), abs(y))) || (nans && isnan(x) && isnan(y))
    end
else
    import Base.isapprox
end

@static if !isdefined(Base, :isabstracttype) # VERSION < v"0.7.0-DEV.3475"
    const isabstracttype = Base.isabstract
    export isabstracttype
end

module TypeUtils
    using Base: parameter_upper_bound, typename
    using Compat: isabstracttype
    const isabstract = isabstracttype
    export isabstract, parameter_upper_bound, typename
end # module TypeUtils

# @view, @views, @__dot__
include("arraymacros.jl")

# julia #18839
import Base.Iterators # TODO deprecate, remove

@static if VERSION < v"0.6.0-dev.2840"
    export IndexStyle, IndexLinear, IndexCartesian
    eval(Expr(:typealias, :IndexStyle, :(Base.LinearIndexing)))
    eval(Expr(:typealias, :IndexLinear, :(Base.LinearFast)))
    eval(Expr(:typealias, :IndexCartesian, :(Base.LinearSlow)))
    IndexStyle{T}(::Type{T}) = Base.linearindexing(T)
    IndexStyle(args...) = Base.linearindexing(args...)
end

if VERSION < v"0.6.0-dev.1653"
    for (fname, felt) in ((:zeros,:zero), (:ones,:one))
        @eval begin
            # allow signature of similar
            Base.$fname(a::AbstractArray, T::Type, dims::Tuple) = fill!(similar(a, T, dims), $felt(T))
            Base.$fname(a::AbstractArray, T::Type, dims...) = fill!(similar(a,T,dims...), $felt(T))
            Base.$fname(a::AbstractArray, T::Type=eltype(a)) = fill!(similar(a,T), $felt(T))
        end
    end
end

# https://github.com/JuliaLang/julia/pull/25646
@static if VERSION < v"0.7.0-DEV.3510"
    # not exported
    # chomp parameter preserved for compatibility with earliear Compat versions
    readline(s::IO=STDIN; chomp::Bool=true, keep::Bool=!chomp) = Base.readline(s; chomp=!keep)
    eachline(s; keep::Bool=false) = Base.eachline(s; chomp=!keep)
end

# https://github.com/JuliaLang/julia/pull/18727
@static if VERSION < v"0.6.0-dev.838"
    Base.convert{T}(::Type{Set{T}}, s::Set{T}) = s
    Base.convert{T}(::Type{Set{T}}, s::Set) = Set{T}(s)
end

# https://github.com/JuliaLang/julia/pull/18082
if VERSION < v"0.6.0-dev.2347"
    Base.isassigned(x::Base.RefValue) = isdefined(x, :x)
end

@static if VERSION < v"0.6.0-dev.735"
    Base.unsafe_trunc{T<:Integer}(::Type{T}, x::Integer) = rem(x, T)
end

# https://github.com/JuliaLang/julia/pull/21346
if VERSION < v"0.6.0-pre.beta.102"
    Base.bswap(z::Complex) = Complex(bswap(real(z)), bswap(imag(z)))
end

# https://github.com/JuliaLang/julia/pull/19449
@static if VERSION < v"0.6.0-dev.1988"
    StringVector(n::Integer) = Vector{UInt8}(n)
else
    using Base: StringVector
end

# https://github.com/JuliaLang/julia/pull/19784
@static if isdefined(Base, :invokelatest)
    # https://github.com/JuliaLang/julia/pull/22646
    if VERSION < v"0.7.0-DEV.1139"
        function invokelatest(f, args...; kwargs...)
            inner() = f(args...; kwargs...)
            Base.invokelatest(inner)
        end
    else
        import Base.invokelatest
    end
else
    function invokelatest(f, args...; kwargs...)
        kw = [Expr(:kw, k, QuoteNode(v)) for (k, v) in kwargs]
        eval(current_module(), Expr(:call, f, map(QuoteNode, args)..., kw...))
    end
end

# https://github.com/JuliaLang/julia/pull/21257
@static if VERSION < v"0.6.0-pre.beta.28"
    collect(A) = collect_indices(indices(A), A)
    collect_indices(::Tuple{}, A) = copy!(Array{eltype(A)}(), A)
    collect_indices(indsA::Tuple{Vararg{Base.OneTo}}, A) =
        copy!(Array{eltype(A)}(map(length, indsA)), A)
    function collect_indices(indsA, A)
        B = Array{eltype(A)}(map(length, indsA))
        copy!(B, CartesianRange(indices(B)), A, CartesianRange(indsA))
    end
else
    const collect = Base.collect
end

# https://github.com/JuliaLang/julia/pull/21197
if VERSION < v"0.7.0-DEV.257"
    # allow the elements of the Cmd to be accessed as an array or iterator
    for f in (:length, :endof, :start, :eachindex, :eltype, :first, :last)
        @eval Base.$f(cmd::Cmd) = $f(cmd.exec)
    end
    for f in (:next, :done, :getindex)
        @eval Base.$f(cmd::Cmd, i) = $f(cmd.exec, i)
    end
end

# https://github.com/JuliaLang/julia/pull/22475
@static if VERSION < v"0.7.0-DEV.843"
    import Base: Val
    (::Type{Val})(x) = (Base.@_pure_meta; Val{x}())
    # Also add methods for Val(x) that were previously Val{x}
    import Base: reshape
    reshape{N}(parent::AbstractArray, ndims::Val{N}) = reshape(parent, Val{N})
    import Base: ntuple
    ntuple{F,N}(f::F, ::Val{N}) = ntuple(f, Val{N})
end

# https://github.com/JuliaLang/julia/pull/22629
if VERSION < v"0.7.0-DEV.848"
    import Base: logdet
    logdet(A) = log(det(A))
end

# https://github.com/JuliaLang/julia/pull/22633
if VERSION < v"0.7.0-DEV.1041"
    import Base.LinAlg: chol, chol!
    chol!(J::UniformScaling, uplo) = UniformScaling(chol!(J.λ, uplo))
    chol(J::UniformScaling, args...) = UniformScaling(chol(J.λ, args...))
end

# https://github.com/JuliaLang/julia/pull/21746
const macros_have_sourceloc = VERSION >= v"0.7-" && length(:(@test).args) == 2

# https://github.com/JuliaLang/julia/pull/22182
module Sys
    const KERNEL = Base.Sys.KERNEL
    @static if VERSION < v"0.7.0-DEV.914"
        isapple(k::Symbol=KERNEL)   = k in (:Darwin, :Apple)
        isbsd(k::Symbol=KERNEL)     = isapple(k) || k in (:FreeBSD, :OpenBSD, :NetBSD, :DragonFly)
        islinux(k::Symbol=KERNEL)   = k == :Linux
        isunix(k::Symbol=KERNEL)    = isbsd(k) || islinux(k)
        iswindows(k::Symbol=KERNEL) = k in (:Windows, :NT)
    else
        import Base.Sys: isapple, isbsd, islinux, isunix, iswindows
    end

    # https://github.com/JuliaLang/julia/pull/25102
    # NOTE: This needs to be in an __init__ because JULIA_HOME is not
    # defined when building system images.
    function __init__()
        global BINDIR = VERSION < v"0.7.0-DEV.3073" ? JULIA_HOME : Base.Sys.BINDIR
    end
end

@static if VERSION < v"0.7.0-DEV.892"
    fieldcount(t) = nfields(t)
    export fieldcount
end

if VERSION < v"0.7.0-DEV.1053"
    Base.read(obj::IO, ::Type{String}) = readstring(obj)
    Base.read(obj::AbstractString, ::Type{String}) = readstring(obj)
    Base.read(obj::Cmd, ::Type{String}) = readstring(obj)
end

# https://github.com/JuliaLang/julia/pull/20005
if VERSION < v"0.7.0-DEV.896"
    Base.InexactError(name::Symbol, T, val) = InexactError()
end

# https://github.com/JuliaLang/julia/pull/22751
if VERSION < v"0.7.0-DEV.924"
    Base.DomainError(val) = DomainError()
    Base.DomainError(val, msg) = DomainError()
end

# https://github.com/JuliaLang/julia/pull/22761
if VERSION < v"0.7.0-DEV.1285"
    Base.OverflowError(msg) = OverflowError()
end

if VERSION < v"0.7.0-DEV.755"
    # This is a hack to only add keyword signature that won't work on all julia versions.
    # However, since we really only need to support a few (0.5, 0.6 and early 0.7) versions
    # this should be good enough.
    # TODO add deprecation warning to switch to StatsBase
    let Tf = typeof(Base.cov), Tkw = Core.Core.kwftype(Tf)
        @eval begin
            @inline function _get_corrected(kws)
                corrected = true
                nkw = length(kws) >> 1
                for i in 1:nkw
                    if kws[i * 2 - 1] !== :corrected
                        Base.kwerr(kws)
                    end
                    corrected = kws[i * 2]
                end
                return corrected::Bool
            end
            if VERSION >= v"0.6"
                (::$Tkw)(kws::Vector{Any}, ::$Tf, x::AbstractVector) = Base.cov(x, _get_corrected(kws))
                (::$Tkw)(kws::Vector{Any}, ::$Tf, X::AbstractVector, Y::AbstractVector) =
                    Base.cov(X, Y, _get_corrected(kws))
            end
            (::$Tkw)(kws::Vector{Any}, ::$Tf, x::AbstractMatrix, vardim::Int) =
                Base.cov(x, vardim, _get_corrected(kws))
            (::$Tkw)(kws::Vector{Any}, ::$Tf, X::AbstractVecOrMat, Y::AbstractVecOrMat,
                     vardim::Int) = Base.cov(X, Y, vardim, _get_corrected(kws))
        end
    end
end

# 0.7.0-DEV.1415
@static if !isdefined(Base, :adjoint)
    const adjoint = ctranspose
    const adjoint! = ctranspose!
    export adjoint, adjoint!
end

# 0.7.0-DEV.1592
@static if !isdefined(Base, :MathConstants)
    @eval module MathConstants
    # All other ones are already exported by Base (so should be already in the users namespace)
    # and will be automatically be in this module.
    export ℯ
    const ℯ = e
    end
    const ℯ = e
    export ℯ
else
    import Base.MathConstants
end

# 0.7.0-DEV.1535
@static if !isdefined(Base, :partialsort)
    const partialsort = select
    const partialsort! = select!
    const partialsortperm = selectperm
    const partialsortperm! = selectperm!
    export partialsort, partialsort!, partialsortperm, partialsortperm!
end

# 0.7.0-DEV.1660
@static if !isdefined(Base, :pairs)
    pairs(collection) = Base.Generator(=>, keys(collection), values(collection))
    pairs(a::Associative) = a

    # 0.6.0-dev+2834
    @static if !isdefined(Iterators, :IndexValue)
        include_string(@__MODULE__, """
            immutable IndexValue{I,A<:AbstractArray}
                data::A
                itr::I
            end
        """)

        Base.length(v::IndexValue)  = length(v.itr)
        Base.indices(v::IndexValue) = indices(v.itr)
        Base.size(v::IndexValue)    = size(v.itr)
        @inline Base.start(v::IndexValue) = start(v.itr)
        Base.@propagate_inbounds function Base.next(v::IndexValue, state)
            indx, n = next(v.itr, state)
            item = v.data[indx]
            (indx => item), n
        end
        @inline Base.done(v::IndexValue, state) = done(v.itr, state)

        Base.eltype{I,A}(::Type{IndexValue{I,A}}) = Pair{eltype(I), eltype(A)}

        Base.iteratorsize{I}(::Type{IndexValue{I}}) = iteratorsize(I)
        Base.iteratoreltype{I}(::Type{IndexValue{I}}) = iteratoreltype(I)

        Base.reverse(v::IndexValue) = IndexValue(v.data, reverse(v.itr))
    else
        const IndexValue = Iterators.IndexValue
    end

    pairs(::IndexLinear,    A::AbstractArray) = IndexValue(A, linearindices(A))
    pairs(::IndexCartesian, A::AbstractArray) = IndexValue(A, CartesianRange(indices(A)))

    Base.keys(a::AbstractArray) = CartesianRange(indices(a))
    Base.keys(a::AbstractVector) = linearindices(a)
    Base.keys(s::IndexStyle, A::AbstractArray, B::AbstractArray...) = eachindex(s, A, B...)

    Base.values(itr) = itr
end

# 0.7.0-DEV.1721
@static if !isdefined(Base, :AbstractRange)
    const AbstractRange = Range
    export AbstractRange
end

if VERSION < v"0.7.0-DEV.1325"
    function Base.rtoldefault(x, y, atol::Real)
        T = isa(x, Type) ? x : typeof(x)
        S = isa(y, Type) ? y : typeof(y)
        rtol = max(Base.rtoldefault(real(T)), Base.rtoldefault(real(S)))
        return atol > 0 ? zero(rtol) : rtol
    end
end


# 0.7.0-DEV.3475
@static if !isdefined(Base, :isconcretetype)
    # 0.7.0-DEV.1775
    @static if !isdefined(Base, :isconcrete)
        const isconcretetype = isleaftype
        const isconcrete = isleaftype # for compatibility with earlier Compat versions
        export isconcrete
    else
        const isconcretetype = isconcrete
    end
    export isconcretetype
end

# 0.7.0-DEV.2005
if VERSION < v"0.7.0-DEV.2005"
    const Mmap = Base.Mmap
    const Test = Base.Test
    @eval module SharedArrays
        if isdefined(Base, :Distributed)
            using Base.Distributed.procs
        else
            using Base.procs
        end
        export SharedArray, SharedMatrix, SharedVector, indexpids, localindexes, sdata,
               procs
    end
    const DelimitedFiles = Base.DataFmt
else
    import Test, SharedArrays, Mmap, DelimitedFiles
end

if VERSION < v"0.7.0-DEV.2575"
    const Dates = Base.Dates
else
    import Dates
end

if VERSION < v"0.7.0-DEV.3382"
    const Libdl = Base.Libdl
else
    import Libdl
end

# https://github.com/JuliaLang/julia/pull/24182
if VERSION < v"0.7.0-DEV.2402"
    const ConvertiblePeriod = Union{Compat.Dates.TimePeriod, Compat.Dates.Week, Compat.Dates.Day}
    const TimeTypeOrPeriod = Union{Compat.Dates.TimeType, Compat.ConvertiblePeriod}

    """
        floor(x::Period, precision::T) where T <: Union{TimePeriod, Week, Day} -> T

    Rounds `x` down to the nearest multiple of `precision`. If `x` and `precision` are different
    subtypes of `Period`, the return value will have the same type as `precision`.

    For convenience, `precision` may be a type instead of a value: `floor(x, Dates.Hour)` is a
    shortcut for `floor(x, Dates.Hour(1))`.

    ```jldoctest
    julia> floor(Dates.Day(16), Dates.Week)
    2 weeks

    julia> floor(Dates.Minute(44), Dates.Minute(15))
    30 minutes

    julia> floor(Dates.Hour(36), Dates.Day)
    1 day
    ```

    Rounding to a `precision` of `Month`s or `Year`s is not supported, as these `Period`s are of
    inconsistent length.
    """
    function Base.floor(x::Compat.ConvertiblePeriod, precision::T) where T <: Compat.ConvertiblePeriod
        Compat.Dates.value(precision) < 1 && throw(DomainError(precision))
        _x, _precision = promote(x, precision)
        return T(_x - mod(_x, _precision))
    end

    """
        ceil(x::Period, precision::T) where T <: Union{TimePeriod, Week, Day} -> T

    Rounds `x` up to the nearest multiple of `precision`. If `x` and `precision` are different
    subtypes of `Period`, the return value will have the same type as `precision`.

    For convenience, `precision` may be a type instead of a value: `ceil(x, Dates.Hour)` is a
    shortcut for `ceil(x, Dates.Hour(1))`.

    ```jldoctest
    julia> ceil(Dates.Day(16), Dates.Week)
    3 weeks

    julia> ceil(Dates.Minute(44), Dates.Minute(15))
    45 minutes

    julia> ceil(Dates.Hour(36), Dates.Day)
    3 days
    ```

    Rounding to a `precision` of `Month`s or `Year`s is not supported, as these `Period`s are of
    inconsistent length.
    """
    function Base.ceil(x::Compat.ConvertiblePeriod, precision::Compat.ConvertiblePeriod)
        f = Base.floor(x, precision)
        return (x == f) ? f : f + precision
    end

    """
        floorceil(x::Period, precision::T) where T <: Union{TimePeriod, Week, Day} -> (T, T)

    Simultaneously return the `floor` and `ceil` of `Period` at resolution `p`.  More efficient
    than calling both `floor` and `ceil` individually.
    """
    function floorceil(x::Compat.ConvertiblePeriod, precision::Compat.ConvertiblePeriod)
        f = Base.floor(x, precision)
        return f, (x == f) ? f : f + precision
    end

    """
        round(x::Period, precision::T, [r::RoundingMode]) where T <: Union{TimePeriod, Week, Day} -> T

    Rounds `x` to the nearest multiple of `precision`. If `x` and `precision` are different
    subtypes of `Period`, the return value will have the same type as `precision`. By default
    (`RoundNearestTiesUp`), ties (e.g., rounding 90 minutes to the nearest hour) will be rounded
    up.

    For convenience, `precision` may be a type instead of a value: `round(x, Dates.Hour)` is a
    shortcut for `round(x, Dates.Hour(1))`.

    ```jldoctest
    julia> round(Dates.Day(16), Dates.Week)
    2 weeks

    julia> round(Dates.Minute(44), Dates.Minute(15))
    45 minutes

    julia> round(Dates.Hour(36), Dates.Day)
    3 days
    ```

    Valid rounding modes for `round(::Period, ::T, ::RoundingMode)` are `RoundNearestTiesUp`
    (default), `RoundDown` (`floor`), and `RoundUp` (`ceil`).

    Rounding to a `precision` of `Month`s or `Year`s is not supported, as these `Period`s are of
    inconsistent length.
    """
    function Base.round(x::Compat.ConvertiblePeriod, precision::Compat.ConvertiblePeriod, r::RoundingMode{:NearestTiesUp})
        f, c = floorceil(x, precision)
        _x, _f, _c = promote(x, f, c)
        return (_x - _f) < (_c - _x) ? f : c
    end

    Base.round(x::Compat.TimeTypeOrPeriod, p::Compat.Dates.Period, r::RoundingMode{:Down}) = Base.floor(x, p)
    Base.round(x::Compat.TimeTypeOrPeriod, p::Compat.Dates.Period, r::RoundingMode{:Up}) = Base.ceil(x, p)

    Base.round(::Compat.TimeTypeOrPeriod, p::Compat.Dates.Period, ::RoundingMode) = throw(DomainError(p))
    Base.round(x::Compat.TimeTypeOrPeriod, p::Compat.Dates.Period) = Base.round(x, p, RoundNearestTiesUp)
    Base.floor(x::Compat.TimeTypeOrPeriod, ::Type{P}) where P <: Compat.Dates.Period = Base.floor(x, oneunit(P))
    Base.ceil(x::Compat.TimeTypeOrPeriod, ::Type{P}) where P <: Compat.Dates.Period = Base.ceil(x, oneunit(P))
    function Base.round(x::Compat.TimeTypeOrPeriod, ::Type{P}, r::RoundingMode=RoundNearestTiesUp) where P <: Compat.Dates.Period
        return Base.round(x, oneunit(P), r)
    end
end

if VERSION < v"0.7.0-DEV.3216"
    const AbstractDateTime = Compat.Dates.TimeType
else
    const AbstractDateTime = Compat.Dates.AbstractDateTime
end

if VERSION < v"0.7.0-DEV.3052"
    const Printf = Base.Printf
else
    import Printf
end

if VERSION < v"0.7.0-DEV.2655"
    @eval module IterativeEigensolvers
        using Base: eigs, svds
        export eigs, svds
    end
elseif VERSION < v"0.7.0-DEV.3019"
    @eval module IterativeEigensolvers
        using IterativeEigenSolvers: eigs, svds
        export eigs, svds
    end
else
    import IterativeEigensolvers
end

@static if VERSION < v"0.7.0-DEV.3449"
    const LinearAlgebra = Base.LinAlg
else
    import LinearAlgebra
end

if VERSION < v"0.7.0-DEV.3389"
    const SparseArrays = Base.SparseArrays
else
    import SparseArrays
end

if VERSION < v"0.7.0-DEV.3406"
    const Random = Base.Random
else
    import Random
end

if VERSION < v"0.7.0-DEV.3589"
    const Markdown = Base.Markdown
else
    import Markdown
end

if VERSION < v"0.7.0-DEV.2609"
    @eval module SuiteSparse
        if Base.USE_GPL_LIBS
            using Compat.SparseArrays: CHOLMOD, SPQR, UMFPACK
        end
        using Compat.SparseArrays: increment, increment!, decrement, decrement!
    end
else
    import SuiteSparse
end

@static if VERSION < v"0.7.0-DEV.3500"
    const REPL = Base.REPL
else
    import REPL
end

if VERSION < v"0.7.0-DEV.3476"
    @eval module Serialization
        import Base.Serializer: serialize, deserialize, SerializationState, serialize_type
        export serialize, deserialize, SerializationState
    end
else
    import Serialization
end

@static if VERSION < v"0.7.0-DEV.4592"
    struct Fix2{F,T} <: Function
        f::F
        x::T
        Fix2(f::F, x::T) where {F,T} = new{F,T}(f, x)
        Fix2(f::Type{F}, x::T) where {F,T} = new{F,T}(f, x)
    end
    (f::Fix2)(y) = f.f(y, f.x)

    Base.:(==)(x) = Fix2(==, x)
    @static if VERSION >= v"0.7.0-DEV.1993"
        Base.isequal(x) = Base.equalto(x)
    else
        Base.isequal(x) = Fix2(isequal, x)
    end
    @static if VERSION >= v"0.7.0-DEV.3272"
        Base.in(x) = Base.occursin(x)
    else
        Base.in(x) = Fix2(in, x)
    end
else
    import Base: Fix2
end
# keep these definitions to be non breaking for 0.6 usage
@static if VERSION < v"0.7.0-DEV.1993"
    const EqualTo{T} = Fix2{typeof(isequal),T}
    export equalto
    equalto(x) = isequal(x)
end
@static if VERSION < v"0.7.0-DEV.3272"
    const OccursIn{T} = Fix2{typeof(in),T}
    export occursin
    occursin(x) = in(x)
end

# PR #26283
if VERSION < v"0.7.0-DEV.4639"
    if isdefined(Base, :occursin)
        import Base: occursin
    else
        export occursin
    end
    occursin(needle, haystack) = contains(haystack, needle)
    if VERSION < v"0.7.0-DEV.3272"
        occursin(r::Regex, s::AbstractString; offset::Integer = 0) = ismatch(r, s, offset)
    else
        occursin(r::Regex, s::AbstractString; offset::Integer = 0) = contains(s, r, offset)
    end
    # PR #22435
    if VERSION < v"0.7.0-DEV.702"
        occursin(needle::Char, haystack::AbstractString) = searchindex(haystack,needle) != 0
    end
end


# 0.7.0-DEV.912
if VERSION < v"0.7.0-DEV.912"
    import Base.*
    (*)(s1::Union{Char,AbstractString}, ss::Union{Char,AbstractString}...) = string(s1, ss...)
end

# 0.7.0-DEV.2318
@static if !isdefined(Base, :BitSet)
    const BitSet = IntSet
    export BitSet
end

# 0.7.0-DEV.2116
@static if VERSION < v"0.7.0-DEV.2116"
    import Compat.SparseArrays: spdiagm
    function spdiagm(kv::Pair...)
        I, J, V = Compat.SparseArrays.spdiagm_internal(last.(kv), first.(kv))
        m = max(Compat.SparseArrays.dimlub(I), Compat.SparseArrays.dimlub(J))
        return sparse(I, J, V, m, m)
    end
end

# 0.7.0-DEV.2161
@static if VERSION < v"0.7.0-DEV.2161"
    import Base: diagm
    function diagm(kv::Pair...)
        T = promote_type(map(x -> eltype(x.second), kv)...)
        n = Base.mapreduce(x -> length(x.second) + abs(x.first), max, kv)
        A = zeros(T, n, n)
        for p in kv
            inds = diagind(A, p.first)
            for (i, val) in enumerate(p.second)
                A[inds[i]] += val
            end
        end
        return A
    end
end

# 0.7.0-DEV.2338
@static if VERSION >= v"0.7.0-DEV.2338"
    import Base64
else
    import Base.Base64
end

@static if VERSION < v"0.7.0-DEV.2377"
    (::Type{Matrix{T}}){T}(s::UniformScaling, dims::Dims{2}) = setindex!(zeros(T, dims), T(s.λ), diagind(dims...))
    (::Type{Matrix{T}}){T}(s::UniformScaling, m::Integer, n::Integer) = Matrix{T}(s, Dims((m, n)))

    (::Type{SparseMatrixCSC{Tv,Ti}}){Tv,Ti}(s::UniformScaling, m::Integer, n::Integer) = SparseMatrixCSC{Tv,Ti}(s, Dims((m, n)))
    (::Type{SparseMatrixCSC{Tv}}){Tv}(s::UniformScaling, m::Integer, n::Integer) = SparseMatrixCSC{Tv}(s, Dims((m, n)))
    (::Type{SparseMatrixCSC{Tv}}){Tv}(s::UniformScaling, dims::Dims{2}) = SparseMatrixCSC{Tv,Int}(s, dims)
    function (::Type{SparseMatrixCSC{Tv,Ti}}){Tv,Ti}(s::UniformScaling, dims::Dims{2})
        @boundscheck first(dims) < 0 && throw(ArgumentError("first dimension invalid ($(first(dims)) < 0)"))
        @boundscheck last(dims) < 0 && throw(ArgumentError("second dimension invalid ($(last(dims)) < 0)"))
        iszero(s.λ) && return spzeros(Tv, Ti, dims...)
        m, n, k = dims..., min(dims...)
        nzval = fill!(Vector{Tv}(k), Tv(s.λ))
        rowval = copy!(Vector{Ti}(k), 1:k)
        colptr = copy!(Vector{Ti}(n + 1), 1:(k + 1))
        for i in (k + 2):(n + 1) colptr[i] = (k + 1) end
        SparseMatrixCSC{Tv,Ti}(dims..., colptr, rowval, nzval)
    end
end
@static if VERSION < v"0.7.0-DEV.2543"
    (::Type{Array{T}}){T}(s::UniformScaling, dims::Dims{2}) = Matrix{T}(s, dims)
    (::Type{Array{T}}){T}(s::UniformScaling, m::Integer, n::Integer) = Matrix{T}(s, m, n)
end
@static if VERSION < v"0.7.0-DEV.2541"
    (::Type{Matrix})(s::UniformScaling{T}, dims...) where {T} = Matrix{T}(s, dims...)
end

# https://github.com/JuliaLang/julia/pull/23271
@static if VERSION < v"0.7.0-DEV.1472"
    Base.IOContext(io::IO, arg1::Pair, arg2::Pair, args::Pair...) = IOContext(IOContext(io, arg1), arg2, args...)
    # needed for ambiguity resolution
    Base.IOContext(io::IOContext, arg1::Pair, arg2::Pair) = IOContext(IOContext(io, arg1), arg2)
end

# 0.7.0-DEV.4527
@static if !isdefined(Base, :UndefInitializer)
    import Base: Array, Matrix, Vector
    @static if isdefined(Base, :Uninitialized)
        useuninit(args) = (Base.uninitialized, args...)
    else
        useuninit(args) = args
    end
    struct UndefInitializer end
    const undef = UndefInitializer()
    export undef, UndefInitializer
    Base.show(io::IO, ::UndefInitializer) =
        print(io, "array initializer with undefined values")
    Array{T}(::UndefInitializer, args...) where {T} = Array{T}(useuninit(args)...)
    Array{T,N}(::UndefInitializer, args...) where {T,N} = Array{T,N}(useuninit(args)...)
    Vector(::UndefInitializer, args...) = Vector(useuninit(args)...)
    Matrix(::UndefInitializer, args...) = Matrix(useuninit(args)...)

    BitArray{N}(::UndefInitializer, args...) where {N} = BitArray{N}(useuninit(args)...)
    BitArray(::UndefInitializer, args...) = BitArray(useuninit(args)...)
end
@static if VERSION < v"0.7.0-DEV.2581"
    export uninitialized, Uninitialized
    const uninitialized = undef
    const Uninitialized = UndefInitializer
end

# 0.7.0-DEV.1499
if VERSION < v"0.7.0-DEV.1499"
    function Base.get(f::Base.Callable, ::Base.EnvHash, k::AbstractString)
        Base.access_env(k->f(), k)
    end
end

# 0.7.0-DEV.2919
@static if !isdefined(Base, :ComplexF16)
    const ComplexF16 = Complex{Float16}
    export ComplexF16
end
@static if !isdefined(Base, :ComplexF32)
    const ComplexF32 = Complex{Float32}
    export ComplexF32
end
@static if !isdefined(Base, :ComplexF64)
    const ComplexF64 = Complex{Float64}
    export ComplexF64
end

# 0.7.0-DEV.2915
module Unicode
    export graphemes, textwidth, isvalid,
           islower, isupper, isalpha, isdigit, isxdigit, isnumeric, isalnum,
           iscntrl, ispunct, isspace, isprint, isgraph,
           lowercase, uppercase, titlecase, lcfirst, ucfirst

    if VERSION < v"0.7.0-DEV.2915"
        # 0.7.0-DEV.1930
        if !isdefined(Base, :textwidth)
            textwidth(c::Char) = charwidth(c)
            textwidth(c::AbstractString) = strwidth(c)
        end

        isnumeric(c::Char) = isnumber(c)
        isassigned(c) = is_assigned_char(c)
        normalize(s::AbstractString; kws...) = normalize_string(s; kws...)
        normalize(s::AbstractString, nf::Symbol) = normalize_string(s, nf)

        # 0.6.0-dev.1404 (https://github.com/JuliaLang/julia/pull/19469)
        if !isdefined(Base, :titlecase)
            titlecase(c::Char) = isascii(c) ? ('a' <= c <= 'z' ? c - 0x20 : c) :
                Char(ccall(:utf8proc_totitle, UInt32, (UInt32,), c))

            function titlecase(s::AbstractString)
                startword = true
                b = IOBuffer()
                for c in s
                    if isspace(c)
                        print(b, c)
                        startword = true
                    else
                        print(b, startword ? titlecase(c) : c)
                        startword = false
                    end
                end
                return String(take!(b))
            end
        end
    else
        using Unicode
        import Unicode: isassigned, normalize # not exported from Unicode module due to conflicts
    end
end

# 0.7.0-DEV.2951
@static if !isdefined(Base, :AbstractDict)
    const AbstractDict = Associative
    export AbstractDict
end

# 0.7.0-DEV.2978
@static if !isdefined(Base, :axes)
    const axes = Base.indices
    # NOTE: Intentionally not exported to avoid conflicts with AxisArrays
    #export axes
end

# 0.7.0-DEV.3137
@static if !isdefined(Base, :Nothing)
    const Nothing = Void
    const Cvoid = Void
    export Nothing, Cvoid
end

@static if !isdefined(Base, :Some)
    import Base: promote_rule, convert
    if VERSION >= v"0.6.0"
        include_string(@__MODULE__, """
            struct Some{T}
                value::T
            end
            promote_rule(::Type{Some{S}}, ::Type{Some{T}}) where {S,T} = Some{promote_type(S, T)}
            promote_rule(::Type{Some{T}}, ::Type{Nothing}) where {T} = Union{Some{T}, Nothing}
            convert(::Type{Some{T}}, x::Some) where {T} = Some{T}(convert(T, x.value))
            convert(::Type{Union{Some{T}, Nothing}}, x::Some) where {T} = convert(Some{T}, x)
            convert(::Type{Union{T, Nothing}}, x::Any) where {T} = convert(T, x)
        """)
    else
        include_string(@__MODULE__, """
            immutable Some{T}
                value::T
            end
            promote_rule{S,T}(::Type{Some{S}}, ::Type{Some{T}}) = Some{promote_type(S, T)}
            promote_rule{T}(::Type{Some{T}}, ::Type{Nothing}) = Union{Some{T}, Nothing}
            convert{T}(::Type{Some{T}}, x::Some) = Some{T}(convert(T, x.value))
            convert{T}(::Type{Union{Some{T}, Nothing}}, x::Some) = convert(Some{T}, x)
            convert{T}(::Type{Union{T, Nothing}}, x::Any) = convert(T, x)
        """)
    end
    convert(::Type{Nothing}, x::Any) = throw(MethodError(convert, (Nothing, x)))
    convert(::Type{Nothing}, x::Nothing) = nothing
    coalesce(x::Any) = x
    coalesce(x::Some) = x.value
    coalesce(x::Nothing) = nothing
    #coalesce(x::Missing) = missing
    coalesce(x::Any, y...) = x
    coalesce(x::Some, y...) = x.value
    coalesce(x::Nothing, y...) = coalesce(y...)
    #coalesce(x::Union{Nothing, Missing}, y...) = coalesce(y...)
    notnothing(x::Any) = x
    notnothing(::Nothing) = throw(ArgumentError("nothing passed to notnothing"))
    export Some, coalesce
else
    import Base: notnothing
end

# 0.7.0-DEV.3155
@static if !isdefined(Base, :pushfirst!)
    const pushfirst! = unshift!
    const popfirst! = shift!
    export pushfirst!, popfirst!
end

# 0.7.0-DEV.3309
@static if VERSION < v"0.7.0-DEV.3309"
    const IteratorSize = Base.iteratorsize
    const IteratorEltype = Base.iteratoreltype
else
    const IteratorSize = Base.IteratorSize
    const IteratorEltype = Base.IteratorEltype
end

# 0.7.0-DEV.3173
@static if !isdefined(Base, :invpermute!)
    const invpermute! = ipermute!
    export invpermute!
end

@static if VERSION < v"0.7.0-DEV.3172"
    Base.replace(s::AbstractString, pat_rep::Pair; count::Integer=typemax(Int)) =
        replace(s, first(pat_rep), last(pat_rep), count)
end

# 0.7.0-DEV.3057
@static if !isdefined(Base, :copyto!)
    const copyto! = Base.copy!
    const unsafe_copyto! = Base.unsafe_copy!
    export copyto!, unsafe_copyto!
end

# 0.7.0-DEV.3272, keep this definition for 0.6 compatibility
@static if VERSION < v"0.7.0-DEV.3272"
    Base.contains(str::AbstractString, r::Regex) = ismatch(r, str)
end

@static if VERSION < v"0.7.0-DEV.3025"
    import Base: convert, ndims, getindex, size, length, eltype,
                 start, next, done, first, last, in, tail
    export CartesianIndices, LinearIndices

    struct CartesianIndices{N,R<:NTuple{N,AbstractUnitRange{Int}}} <: AbstractArray{CartesianIndex{N},N}
        indices::R
    end

    CartesianIndices(::Tuple{}) = CartesianIndices{0,typeof(())}(())
    CartesianIndices(inds::NTuple{N,AbstractUnitRange{Int}}) where {N} =
        CartesianIndices{N,typeof(inds)}(inds)
    CartesianIndices(inds::Vararg{AbstractUnitRange{Int},N}) where {N} =
        CartesianIndices(inds)
    CartesianIndices(inds::NTuple{N,AbstractUnitRange{<:Integer}}) where {N} =
        CartesianIndices(map(r->convert(AbstractUnitRange{Int}, r), inds))
    CartesianIndices(inds::Vararg{AbstractUnitRange{<:Integer},N}) where {N} =
        CartesianIndices(inds)

    CartesianIndices(index::CartesianIndex) = CartesianIndices(index.I)
    CartesianIndices(sz::NTuple{N,<:Integer}) where {N} = CartesianIndices(map(Base.OneTo, sz))
    CartesianIndices(inds::NTuple{N,Union{<:Integer,AbstractUnitRange{<:Integer}}}) where {N} =
        CartesianIndices(map(i->first(i):last(i), inds))

    CartesianIndices(A::AbstractArray) = CartesianIndices(axes(A))

    convert(::Type{Tuple{}}, R::CartesianIndices{0}) = ()
    convert(::Type{NTuple{N,AbstractUnitRange{Int}}}, R::CartesianIndices{N}) where {N} =
        R.indices

    convert(::Type{NTuple{N,AbstractUnitRange}}, R::CartesianIndices{N}) where {N} =
        convert(NTuple{N,AbstractUnitRange{Int}}, R)
    convert(::Type{NTuple{N,UnitRange{Int}}}, R::CartesianIndices{N}) where {N} =
        UnitRange{Int}.(convert(NTuple{N,AbstractUnitRange}, R))
    convert(::Type{NTuple{N,UnitRange}}, R::CartesianIndices{N}) where {N} =
        UnitRange.(convert(NTuple{N,AbstractUnitRange}, R))
    convert(::Type{Tuple{Vararg{AbstractUnitRange{Int}}}}, R::CartesianIndices{N}) where {N} =
        convert(NTuple{N,AbstractUnitRange{Int}}, R)
    convert(::Type{Tuple{Vararg{AbstractUnitRange}}}, R::CartesianIndices) =
        convert(Tuple{Vararg{AbstractUnitRange{Int}}}, R)
    convert(::Type{Tuple{Vararg{UnitRange{Int}}}}, R::CartesianIndices{N}) where {N} =
        convert(NTuple{N,UnitRange{Int}}, R)
    convert(::Type{Tuple{Vararg{UnitRange}}}, R::CartesianIndices) =
        convert(Tuple{Vararg{UnitRange{Int}}}, R)

    # AbstractArray implementation
    Base.IndexStyle(::Type{CartesianIndices{N,R}}) where {N,R} = IndexCartesian()
    @inline Base.getindex(iter::CartesianIndices{N,R}, I::Vararg{Int, N}) where {N,R} = CartesianIndex(first.(iter.indices) .- 1 .+ I)

    ndims(R::CartesianIndices) = ndims(typeof(R))
    ndims(::Type{CartesianIndices{N}}) where {N} = N
    ndims(::Type{CartesianIndices{N,TT}}) where {N,TT} = N

    eltype(R::CartesianIndices) = eltype(typeof(R))
    eltype(::Type{CartesianIndices{N}}) where {N} = CartesianIndex{N}
    eltype(::Type{CartesianIndices{N,TT}}) where {N,TT} = CartesianIndex{N}
    Base.iteratorsize(::Type{<:CartesianIndices}) = Base.HasShape()

    @inline function start(iter::CartesianIndices)
        iterfirst, iterlast = first(iter), last(iter)
        if Base.any(map(>, iterfirst.I, iterlast.I))
            return iterlast+1
        end
        iterfirst
    end
    @inline function next(iter::CartesianIndices, state)
        state, CartesianIndex(inc(state.I, first(iter).I, last(iter).I))
    end
    # increment & carry
    @inline inc(::Tuple{}, ::Tuple{}, ::Tuple{}) = ()
    @inline inc(state::Tuple{Int}, start::Tuple{Int}, stop::Tuple{Int}) = (state[1]+1,)
    @inline function inc(state, start, stop)
        if state[1] < stop[1]
            return (state[1]+1,Base.tail(state)...)
        end
        newtail = inc(Base.tail(state), Base.tail(start), Base.tail(stop))
        (start[1], newtail...)
    end
    @inline done(iter::CartesianIndices, state) = state.I[end] > last(iter.indices[end])

    # 0-d cartesian ranges are special-cased to iterate once and only once
    start(iter::CartesianIndices{0}) = false
    next(iter::CartesianIndices{0}, state) = CartesianIndex(), true
    done(iter::CartesianIndices{0}, state) = state

    size(iter::CartesianIndices) = map(dimlength, first(iter).I, last(iter).I)
    dimlength(start, stop) = stop-start+1

    length(iter::CartesianIndices) = Base.prod(size(iter))

    first(iter::CartesianIndices) = CartesianIndex(map(first, iter.indices))
    last(iter::CartesianIndices)  = CartesianIndex(map(last, iter.indices))

    @inline function in(i::CartesianIndex{N}, r::CartesianIndices{N}) where {N}
        _in(true, i.I, first(r).I, last(r).I)
    end
    _in(b, ::Tuple{}, ::Tuple{}, ::Tuple{}) = b
    @inline _in(b, i, start, stop) = _in(b & (start[1] <= i[1] <= stop[1]), tail(i), tail(start), tail(stop))

    struct LinearIndices{N,R<:NTuple{N,AbstractUnitRange{Int}}} <: AbstractArray{Int,N}
        indices::R
    end

    LinearIndices(inds::CartesianIndices{N,R}) where {N,R} = LinearIndices{N,R}(inds.indices)
    LinearIndices(::Tuple{}) = LinearIndices(CartesianIndices(()))
    LinearIndices(inds::NTuple{N,AbstractUnitRange{Int}}) where {N} = LinearIndices(CartesianIndices(inds))
    LinearIndices(inds::Vararg{AbstractUnitRange{Int},N}) where {N} = LinearIndices(CartesianIndices(inds))
    LinearIndices(inds::NTuple{N,AbstractUnitRange{<:Integer}}) where {N} = LinearIndices(CartesianIndices(inds))
    LinearIndices(inds::Vararg{AbstractUnitRange{<:Integer},N}) where {N} = LinearIndices(CartesianIndices(inds))
    LinearIndices(index::CartesianIndex) = LinearIndices(CartesianIndices(index))
    LinearIndices(sz::NTuple{N,<:Integer}) where {N} = LinearIndices(CartesianIndices(sz))
    LinearIndices(inds::NTuple{N,Union{<:Integer,AbstractUnitRange{<:Integer}}}) where {N} = LinearIndices(CartesianIndices(inds))
    LinearIndices(A::AbstractArray) = LinearIndices(CartesianIndices(A))

    # AbstractArray implementation
    Base.IndexStyle(::Type{LinearIndices{N,R}}) where {N,R} = IndexCartesian()
    Compat.axes(iter::LinearIndices{N,R}) where {N,R} = iter.indices
    Base.size(iter::LinearIndices{N,R}) where {N,R} = length.(iter.indices)
    @inline function Base.getindex(iter::LinearIndices{N,R}, I::Vararg{Int, N}) where {N,R}
        dims = length.(iter.indices)
        #without the inbounds, this is slower than Base._sub2ind(iter.indices, I...)
        @inbounds result = reshape(1:Base.prod(dims), dims)[(I .- first.(iter.indices) .+ 1)...]
        return result
    end
elseif VERSION < v"0.7.0-DEV.3395"
    Base.size(iter::LinearIndices{N,R}) where {N,R} = length.(iter.indices)
end

@static if !isdefined(Base, Symbol("@info"))
    macro info(msg, args...)
        return :(info($(esc(msg)), prefix = "Info: "))
    end
else
    @eval const $(Symbol("@info")) = Base.$(Symbol("@info"))
end
@static if !isdefined(Base, Symbol("@warn"))
    macro warn(msg, args...)
        return :(warn($(esc(msg)), prefix = "Warning: "))
    end
else
    @eval const $(Symbol("@warn")) = Base.$(Symbol("@warn"))
end

const DEBUG = Ref(false) # debug printing off by default, as on 0.7
enable_debug(x::Bool) = DEBUG[] = x
@static if !isdefined(Base, Symbol("@debug"))
    function debug(msg)
        DEBUG[] || return
        buf = Base.IOBuffer()
        iob = Base.redirect(IOContext(buf, STDERR), Base.log_info_to, :debug)
        print_with_color(:blue, iob, "Debug: "; bold = true)
        Base.println_with_color(:blue, iob, chomp(string(msg)))
        print(STDERR, String(take!(buf)))
        return
    end
    macro debug(msg, args...)
        return :(debug($(esc(msg))))
    end
else
    @eval const $(Symbol("@debug")) = Base.$(Symbol("@debug"))
end
@static if !isdefined(Base, Symbol("@error"))
    function _error(msg)
        buf = Base.IOBuffer()
        iob = Base.redirect(IOContext(buf, STDERR), Base.log_error_to, :error)
        print_with_color(Base.error_color(), iob, "Error: "; bold = true)
        Base.println_with_color(Base.error_color(), iob, chomp(string(msg)))
        print(STDERR, String(take!(buf)))
        return
    end
    macro error(msg, args...)
        return :(_error($(esc(msg))))
    end
else
    @eval const $(Symbol("@error")) = Base.$(Symbol("@error"))
end

# 0.7.0-DEV.3415
if !isdefined(Base, :findall)
    const findall = find
    export findall
end

@static if !isdefined(Base, :argmin)
    if VERSION >= v"0.7.0-DEV.1660" # indmin/indmax return key
        const argmin = indmin
        const argmax = indmax
    else
        argmin(x::AbstractArray) = CartesianIndex(ind2sub(x, indmin(x)))
        argmin(x::AbstractVector) = indmin(x)
        argmin(x::Associative) = first(Iterators.drop(keys(x), indmin(values(x))-1))
        argmax(x::AbstractArray) = CartesianIndex(ind2sub(x, indmax(x)))
        argmax(x::AbstractVector) = indmax(x)
        argmax(x::Associative) = first(Iterators.drop(keys(x), indmax(values(x))-1))
    end
    export argmin, argmax
end

@static if !isdefined(Base, :parentmodule)
    parentmodule(m::Module) = Base.module_parent(m)
    parentmodule(f::Function) = Base.function_module(f)
    parentmodule(@nospecialize(f), @nospecialize(t)) = Base.function_module(f, t)
    parentmodule(t::DataType) = Base.datatype_module(t)
    parentmodule(t::UnionAll) = Base.datatype_module(Base.unwrap_unionall(t))
    export parentmodule
end

@static if !isdefined(Base, :codeunits)
    codeunits(s::String) = Vector{UInt8}(s)
    ncodeunits(s::Union{String,SubString{String}}) = sizeof(s)
    codeunits(s::SubString{String}) = view(codeunits(s.string),1+s.offset:s.offset+sizeof(s))
    export codeunits, ncodeunits
end

@static if !isdefined(Base, :nameof)
    nameof(m::Module) = Base.module_name(m)
    nameof(f::Function) = Base.function_name(f)
    nameof(t::Union{DataType,UnionAll}) = Base.datatype_name(t)
    export nameof
end

# 0.7.0-DEV.3469
@static if !isdefined(Base, :GC)
    @eval module GC
        using Base: gc
        const enable = Base.gc_enable
    end
    export GC
end

if VERSION < v"0.7.0-DEV.2954"
    const Distributed = Base.Distributed
else
    import Distributed
end

@static if VERSION < v"0.7.0-DEV.3656"
    const Pkg = Base.Pkg
else
    import Pkg
end

@static if VERSION < v"0.7.0-DEV.3630"
    @eval module InteractiveUtils
        using Base: @code_llvm, @code_lowered, @code_native, @code_typed,
                    @code_warntype, @edit, @functionloc, @less, @which,
                    apropos, code_llvm, code_native, code_warntype, edit,
                    less, methodswith, subtypes, versioninfo
        export @code_llvm, @code_lowered, @code_native, @code_typed,
               @code_warntype, @edit, @functionloc, @less, @which,
               apropos, code_llvm, code_native, code_warntype, edit,
               less, methodswith, subtypes, versioninfo

        @static if VERSION >= v"0.7.0-DEV.2582"
            using Base: varinfo
            export varinfo
        else
            const varinfo = whos
            export varinfo
        end
    end
else
    import InteractiveUtils
end

@static if VERSION < v"0.7.0-DEV.3724"
    const LibGit2 = Base.LibGit2
else
    import LibGit2
end

# 0.7.0-DEV.2695
@static if !isdefined(Base, :AbstractDisplay)
    const AbstractDisplay = Display
    export AbstractDisplay
end

# 0.7.0-DEV.3481
@static if !isdefined(Base, :bytesavailable)
    const bytesavailable = nb_available
    export bytesavailable
end

# 0.7.0-DEV.3583
@static if !isdefined(Base, :lastindex)
    const lastindex = endof
    export lastindex
    firstindex(a::AbstractArray) = (Base.@_inline_meta; first(linearindices(a)))
    firstindex(c::Char) = 1
    firstindex(c::Number) = 1
    firstindex(p::Pair) = 1
    firstindex(cmd::Cmd) = firstindex(cmd.exec)
    firstindex(s::AbstractString) = 1
    firstindex(t::Tuple) = 1
    export firstindex
end

# 0.7.0-DEV.3585
@static if !isdefined(Base, :printstyled)
    printstyled(io::IO, msg...; bold=false, color=:normal) =
        Base.print_with_color(color, io, msg...; bold=bold)
    printstyled(msg...; bold=false, color=:normal) =
        Base.print_with_color(color, STDOUT, msg...; bold=bold)
    export printstyled
end

# 0.7.0-DEV.3455
@static if !isdefined(Base, :hasmethod)
    const hasmethod = method_exists
    export hasmethod
end
@static if !isdefined(Base, :objectid)
    const objectid = object_id
    export objectid
end

@static if VERSION >= v"0.7.0-DEV.3272"
    findnext(xs...) = Base.findnext(xs...)
    findfirst(xs...) = Base.findfirst(xs...)
    findprev(xs...) = Base.findprev(xs...)
    findlast(xs...) = Base.findlast(xs...)
else
    zero2nothing(x::Integer) = x == 0 ? nothing : x
    zero2nothing(x::AbstractUnitRange{<:Integer}) = x == 0:-1 ? nothing : x
    zero2nothing(x) = x

    findnext(xs...) = zero2nothing(Base.findnext(xs...))
    findfirst(xs...) = zero2nothing(Base.findfirst(xs...))
    findprev(xs...) = zero2nothing(Base.findprev(xs...))
    findlast(xs...) = zero2nothing(Base.findlast(xs...))

    Base.findnext(r::Regex, s::AbstractString, idx::Integer) = search(s, r, idx)
    Base.findfirst(r::Regex, s::AbstractString) = search(s, r)
    Base.findnext(c::Fix2{typeof(isequal),Char}, s::AbstractString, i::Integer) = search(s, c.x, i)
    Base.findfirst(c::Fix2{typeof(isequal),Char}, s::AbstractString) = search(s, c.x)
    Base.findnext(b::Fix2{typeof(isequal),<:Union{Int8,UInt8}}, a::Vector{<:Union{Int8,UInt8}}, i::Integer) =
        search(a, b.x, i)
    Base.findfirst(b::Fix2{typeof(isequal),<:Union{Int8,UInt8}}, a::Vector{<:Union{Int8,UInt8}}) =
        search(a, b.x)

    Base.findnext(c::Fix2{typeof(in),<:Union{Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}},
             s::AbstractString, i::Integer) =
        search(s, c.x, i)
    Base.findfirst(c::Fix2{typeof(in),<:Union{Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}},
              s::AbstractString) =
        search(s, c.x)
    Base.findnext(t::AbstractString, s::AbstractString, i::Integer) = search(s, t, i)
    Base.findfirst(t::AbstractString, s::AbstractString) = search(s, t)

    Base.findfirst(delim::Fix2{typeof(isequal),UInt8}, buf::Base.IOBuffer) = search(buf, delim.x)

    Base.findprev(c::Fix2{typeof(isequal),Char}, s::AbstractString, i::Integer) = rsearch(s, c.x, i)
    Base.findlast(c::Fix2{typeof(isequal),Char}, s::AbstractString) = rsearch(s, c.x)
    Base.findprev(b::Fix2{typeof(isequal),<:Union{Int8,UInt8}}, a::Vector{<:Union{Int8,UInt8}}, i::Integer) =
        rsearch(a, b.x, i)
    Base.findlast(b::Fix2{typeof(isequal),<:Union{Int8,UInt8}}, a::Vector{<:Union{Int8,UInt8}}) =
        rsearch(a, b.x)

    Base.findprev(c::Fix2{typeof(in),<:Union{Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}},
             s::AbstractString, i::Integer) = rsearch(s, c.x, i)
    Base.findlast(c::Fix2{typeof(in),<:Union{Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}},
             s::AbstractString) = rsearch(s, c.x)
    Base.findprev(t::AbstractString, s::AbstractString, i::Integer) = rsearch(s, t, i)
    Base.findlast(t::AbstractString, s::AbstractString) = rsearch(s, t)

    findall(b::Fix2{typeof(in)}, a) = findin(a, b.x)
    # To fix ambiguity
    findall(b::Fix2{typeof(in)}, a::Number) = a in b.x ? [1] : Vector{Int}()
end

@static if VERSION < v"0.7.0-DEV.4047" #26089
    showable(mime, x) = mimewritable(mime, x)
    export showable
end

@static if VERSION < v"0.7.0-DEV.4010" #25990
    Base.repr(mime::Union{AbstractString,MIME}, x) = reprmime(mime, x)
end

# https://github.com/JuliaLang/julia/pull/25647
@static if VERSION < v"0.7.0-DEV.3526"
    names(m; all=false, imported=false) = Base.names(m, all, imported)
else
    import Base: names
end

if VERSION >= v"0.7.0-DEV.3666"
    import UUIDs
else
    @eval module UUIDs
        import ..Random: uuid1, uuid4, uuid_version, UUID
        export uuid1, uuid4, uuid_version, UUID
    end
end

# https://github.com/JuliaLang/julia/pull/26670
@static if VERSION < v"0.7.0-DEV.4062"
    trunc(x; digits = digits, base = base) = Base.trunc(x, digits, base)
    floor(x; digits = digits, base = base) = Base.floor(x, digits, base)
    ceil(x; digits = digits, base = base) = Base.ceil(x, digits, base)
    function round(x; digits = nothing, sigdigits = nothing, base = base)
        if digits === nothing
            if sigdigits === nothing
                Base.round(x, 0, base)
            else
                Base.signif(x, sigdigits, base)
            end
        else
            sigdigits === nothing || throw(AgrumentError("`round` cannot use both `digits` and `sigdigits` arguments"))
            Base.round(x, digits, base)
        end
    end
elseif VERSION < v"0.7.0-DEV.4804"
    trunc(x; digits = digits, base = base) = Base.trunc(x, digits, base = base)
    floor(x; digits = digits, base = base) = Base.floor(x, digits, base = base)
    ceil(x; digits = digits, base = base) = Base.ceil(x, digits, base = base)
    function round(x; digits = nothing, sigdigits = nothing, base = base)
        if digits === nothing
            if sigdigits === nothing
                Base.round(x, 0, base = base)
            else
                Base.signif(x, sigdigits, base = base)
            end
        else
            sigdigits === nothing || throw(AgrumentError("`round` cannot use both `digits` and `sigdigits` arguments"))
            Base.round(x, digits, base = base)
        end
    end
else
    trunc(x; digits = digits, base = base) = Base.trunc(x, digits = digits, base = base)
    floor(x; digits = digits, base = base) = Base.floor(x, digits = digits, base = base)
    ceil(x; digits = digits, base = base) = Base.ceil(x, digits = digits, base = base)
    round(x; digits = nothing, sigdigits = nothing, base = base) = Base.round(x, digits = digits, sigdigits = sigdigits, base = base)
end

# compatibiltiy with https://github.com/JuliaLang/julia/pull/26156
trunc(x, digits; base = base) = trunc(x, digits = digits, base = base)
floor(x, digits; base = base) = floor(x, digits = digits, base = base)
ceil(x, digits; base = base) = ceil(x, digits = digits, base = base)
round(x, digits; base = base) = round(x, digits = digits, base = base)
signif(x, digits; base = base) = round(x, sigdigits = digits, base = base)

# https://github.com/JuliaLang/julia/pull/25872
if VERSION < v"0.7.0-DEV.3734"
    if isdefined(Base, :open_flags)
        import Base.open_flags
    else
        # copied from Base:
        function open_flags(; read=nothing, write=nothing, create=nothing, truncate=nothing, append=nothing)
            if write === true && read !== true && append !== true
                create   === nothing && (create   = true)
                truncate === nothing && (truncate = true)
            end
            if truncate === true || append === true
                write  === nothing && (write  = true)
                create === nothing && (create = true)
            end
            write    === nothing && (write    = false)
            read     === nothing && (read     = !write)
            create   === nothing && (create   = false)
            truncate === nothing && (truncate = false)
            append   === nothing && (append   = false)
            return (read, write, create, truncate, append)
        end
    end
    function IOBuffer(
            data::Union{AbstractVector{UInt8},Nothing}=nothing;
            read::Union{Bool,Nothing}=data === nothing ? true : nothing,
            write::Union{Bool,Nothing}=data === nothing ? true : nothing,
            truncate::Union{Bool,Nothing}=data === nothing ? true : nothing,
            maxsize::Integer=typemax(Int),
            sizehint::Union{Integer,Nothing}=nothing)
        flags = open_flags(read=read, write=write, append=nothing, truncate=truncate)
        if maxsize < 0
            throw(ArgumentError("negative maxsize: $(maxsize)"))
        end
        if data !== nothing
            if sizehint !== nothing
                sizehint!(data, sizehint)
            end
            buf = Base.IOBuffer(data, flags[1], flags[2], Int(maxsize))
        else
            size = sizehint !== nothing ? Int(sizehint) : maxsize != typemax(Int) ? Int(maxsize) : 32
            buf = Base.IOBuffer(StringVector(size), flags[1], flags[2], Int(maxsize))
            buf.data[:] = 0
        end
        if flags[4] # flags.truncate
            buf.size = 0
        end
        return buf
    end
end

@static if VERSION < v"0.7.0-DEV.3986"
    const LinRange = Base.LinSpace
    export LinRange

    function range(start; step=nothing, stop=nothing, length=nothing)
        have_step = step !== nothing
        have_stop = stop !== nothing
        have_length = length !== nothing

        if !(have_stop || have_length)
            throw(ArgumentError("At least one of `length` or `stop` must be specified"))
        elseif have_step && have_stop && have_length
            throw(ArgumentError("Too many arguments specified; try passing only one of `stop` or `length`"))
        elseif start === nothing
            throw(ArgumentError("Can't start a range at `nothing`"))
        end

        if have_stop && !have_length
            return have_step ? (start:step:stop) : (start:stop)
        elseif have_length && !have_stop
            return have_step ? Base.range(start, step, length) : Base.range(start, length)
        elseif !have_step
            return linspace(start, stop, length)
        end
    end
else
    import Base: range, LinRange
end

@static if VERSION < v"0.7.0-DEV.3995"
    cp(src::AbstractString, dst::AbstractString; force::Bool=false, follow_symlinks::Bool=false) =
        Base.cp(src, dst; remove_destination = force, follow_symlinks = follow_symlinks)
    mv(src::AbstractString, dst::AbstractString; force::Bool=false) =
        Base.mv(src, dst; remove_destination = force)
end

if VERSION < v"0.7.0-DEV.3972"
    function indexin(a, b::AbstractArray)
        inds = keys(b)
        bdict = Dict{eltype(b),eltype(inds)}()
        for (val, ind) in zip(b, inds)
            get!(bdict, val, ind)
        end
        return Union{eltype(inds), Nothing}[
             get(bdict, i, nothing) for i in a
         ]
    end
else
    const indexin = Base.indexin
end

if VERSION < v"0.7.0-DEV.4585"
    export isuppercase, islowercase, uppercasefirst, lowercasefirst
    const isuppercase = isupper
    const islowercase = islower
    const uppercasefirst = ucfirst
    const lowercasefirst = lcfirst
end

if VERSION < v"0.7.0-DEV.4064"
    for f in (:mean, :cumsum, :cumprod, :sum, :prod, :maximum, :minimum, :all, :any, :median)
        @eval begin
            $f(a::AbstractArray; dims=nothing) =
                dims===nothing ? Base.$f(a) : Base.$f(a, dims)
        end
    end
    for f in (:sum, :prod, :maximum, :minimum, :all, :any, :accumulate)
        @eval begin
            $f(f, a::AbstractArray; dims=nothing) =
                dims===nothing ? Base.$f(f, a) : Base.$f(f, a, dims)
        end
    end
    for f in (:findmax, :findmin)
        @eval begin
            $f(a::AbstractVector; dims=nothing) =
                dims===nothing ? Base.$f(a) : Base.$f(a, dims)
            function $f(a::AbstractArray; dims=nothing)
                vs, inds = dims===nothing ? Base.$f(a) : Base.$f(a, dims)
                cis = CartesianIndices(a)
                return (vs, map(i -> cis[i], inds))
            end
        end
    end
    # TODO add deprecation warning to switch to StatsBase to var and std
    for f in (:var, :std, :sort)
        @eval begin
            $f(a::AbstractArray; dims=nothing, kwargs...) =
                dims===nothing ? Base.$f(a; kwargs...) : Base.$f(a, dims; kwargs...)
        end
    end
    for f in (:cumsum!, :cumprod!)
        @eval $f(out, a; dims=nothing) =
            dims===nothing ? Base.$f(out, a) : Base.$f(out, a, dims)
    end
end
if VERSION < v"0.7.0-DEV.4064"
    # TODO add deprecation warning to switch to StatsBase to varm, cov, and cor
    varm(A::AbstractArray, m; dims=nothing, kwargs...) =
        dims===nothing ? Base.varm(A, m; kwargs...) : Base.varm(A, m, dims; kwargs...)
    if VERSION < v"0.7.0-DEV.755"
        cov(a::AbstractMatrix; dims=1, corrected=true) = Base.cov(a, dims, corrected)
        cov(a::AbstractVecOrMat, b::AbstractVecOrMat; dims=1, corrected=true) =
            Base.cov(a, b, dims, corrected)
    else
        cov(a::AbstractMatrix; dims=nothing, kwargs...) =
            dims===nothing ? Base.cov(a; kwargs...) : Base.cov(a, dims; kwargs...)
        cov(a::AbstractVecOrMat, b::AbstractVecOrMat; dims=nothing, kwargs...) =
            dims===nothing ? Base.cov(a, b; kwargs...) : Base.cov(a, b, dims; kwargs...)
    end
    cor(a::AbstractMatrix; dims=nothing) = dims===nothing ? Base.cor(a) : Base.cor(a, dims)
    cor(a::AbstractVecOrMat, b::AbstractVecOrMat; dims=nothing) =
        dims===nothing ? Base.cor(a, b) : Base.cor(a, b, dims)
    mapreduce(f, op, a::AbstractArray; dims=nothing) =
        dims===nothing ? Base.mapreduce(f, op, a) : Base.mapreducedim(f, op, a, dims)
    mapreduce(f, op, v0, a::AbstractArray; dims=nothing) =
        dims===nothing ? Base.mapreduce(f, op, v0, a) : Base.mapreducedim(f, op, a, dims, v0)
    reduce(op, a::AbstractArray; dims=nothing) =
        dims===nothing ? Base.reduce(op, a) : Base.reducedim(op, a, dims)
    reduce(op, v0, a::AbstractArray; dims=nothing) =
        dims===nothing ? Base.reduce(op, v0, a) : Base.reducedim(op, a, dims, v0)
    accumulate!(op, out, a; dims=nothing) =
        dims===nothing ? Base.accumulate!(op, out, a) : Base.accumulate!(op, out, a, dims)
end
if VERSION < v"0.7.0-DEV.4534"
    reverse(a::AbstractArray; dims=nothing) =
        dims===nothing ? Base.reverse(a) : Base.flipdim(a, dims)
end
if VERSION < v"0.7.0-DEV.4738"
    Base.squeeze(A; dims=error("squeeze: keyword argument dims not assigned")) = squeeze(A, dims)
end

if !isdefined(Base, :selectdim) # 0.7.0-DEV.3976
    export selectdim
    @inline selectdim(A::AbstractArray, d::Integer, i) = _selectdim(A, d, i, Base.setindex(map(Base.Slice, axes(A)), i, d))
    @noinline function _selectdim(A, d, i, idxs)
        d >= 1 || throw(ArgumentError("dimension must be ≥ 1"))
        nd = ndims(A)
        d > nd && (i == 1 || throw(BoundsError(A, (ntuple(k->Colon(),d-1)..., i))))
        return view(A, idxs...)
    end
end

if VERSION < v"0.7.0-DEV.2337"
    # qr doesn't take the full keyword anymore since 0.7.0-DEV.5211; we still support it
    # here to avoid unneccesary breakage
    if VERSION < v"0.7.0-DEV.843"
        qr(A::Union{Number,AbstractMatrix}, pivot::Union{Val{false},Val{true}}=Val(false); full=false) =
            Base.qr(A, typeof(pivot), thin=!full)
    else
        qr(A::Union{Number,AbstractMatrix}, pivot::Union{Val{false},Val{true}}=Val(false); full=false) =
            Base.qr(A, pivot, thin=!full)
    end
else
    using LinearAlgebra: qr
end

# rmul! (NOTE: Purposefully not exported)
if VERSION < v"0.7.0-DEV.3563" # scale! not deprecated
    if VERSION >= v"0.7.0-DEV.3449" # LinearAlgebra in the stdlib
        using LinearAlgebra: UnitUpperTriangular, UnitLowerTriangular, scale!
    else
        using Base.LinAlg: UnitUpperTriangular, UnitLowerTriangular, scale!
    end
    const Triangle = Union{UpperTriangular, UnitUpperTriangular,
                           LowerTriangular, UnitLowerTriangular}
    if VERSION < v"0.7.0-DEV.3204" # A_mul_B! not deprecated
        rmul!(A::AbstractMatrix, B::Triangle) = A_mul_B!(A, A, B)
    else
        rmul!(A::AbstractMatrix, B::Triangle) = mul!(A, A, B)
    end
    rmul!(A::AbstractArray, s::Number) = scale!(A, s)
    rmul!(A::AbstractMatrix, D::Diagonal) = scale!(A, D.diag)
    rmul!(A::Diagonal, B::Diagonal) = Diagonal(A.diag .*= B.diag)
    rmul!(A::Triangle, B::Diagonal) = typeof(A)(rmul!(A.data, B))
elseif v"0.7.0-DEV.3563" <= VERSION < v"0.7.0-DEV.3665" # scale! -> mul1!
    using LinearAlgebra: mul1!
    const rmul! = mul1!
elseif VERSION >= v"0.7.0-DEV.3665" # mul1! -> rmul!
    using LinearAlgebra: rmul!
end

@static if VERSION < v"0.7.0-DEV.3936"
    Base.fetch(t::Task) = wait(t)
else
    import Base: fetch
end

# https://github.com/JuliaLang/julia/pull/27077
@static if VERSION < v"0.7.0-DEV.5087"
    export isletter
    const isletter = isalpha
end

# https://github.com/JuliaLang/julia/pull/26850
if !isdefined(Base, :isbitstype) # 0.7.0-DEV.4905
    export isbitstype
    isbitstype(::Type{T}) where {T} = isbits(T)
end

# 0.7.0-DEV.4762
@static if !isdefined(Base, Symbol("@cfunction"))
    macro cfunction(f, rt, tup)
        :(Base.cfunction($f, $rt, Tuple{$tup...}))
    end
    export @cfunction
end

include("deprecated.jl")

end # module Compat
