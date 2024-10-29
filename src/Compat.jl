module Compat

include("compatmacro.jl")

# https://github.com/JuliaLang/julia/pull/47679
if VERSION < v"1.11.0-DEV.1562"
    allunique(f, xs) = Base.allunique(Base.Generator(f, xs))
    function allunique(f::F, t::Tuple) where {F}
        length(t) < 2 && return true
        length(t) < 32 || return Base._hashed_allunique(Base.Generator(f, t))
        return Base.allunique(map(f, t))
    end
    allunique(args...) = Base.allunique(args...)

    allequal(f, xs) = Base.allequal(Base.Generator(f, xs))
    function allequal(f, xs::Tuple)
        length(xs) <= 1 && return true
        f1 = f(xs[1])
        for x in Base.tail(xs)
            isequal(f1, f(x)) || return false
        end
        return true
    end
    allequal(args...) = Base.allequal(args...)
else
    const allunique = Base.allunique
    const allequal = Base.allequal
end

# https://github.com/JuliaLang/julia/pull/47354
@static if VERSION < v"1.11.0-DEV.1579"
    include("Iterators.jl")
else
    const Iterators = Base.Iterators
end

# https://github.com/JuliaLang/julia/pull/39071
if !isdefined(Base, :logrange)  # VERSION < v"1.12.0-DEV.2" or appropriate 1.11.x after backporting

    export logrange

    @doc """
        logrange(start, stop, length)
        logrange(start, stop; length)

    Construct a specialized array whose elements are spaced logarithmically
    between the given endpoints. That is, the ratio of successive elements is
    a constant, calculated from the length.

    This is similar to `geomspace` in Python. Unlike `PowerRange` in Mathematica,
    you specify the number of elements not the ratio.
    Unlike `logspace` in Python and Matlab, the `start` and `stop` arguments are
    always the first and last elements of the result, not powers applied to some base.

    # Examples
    ```
    julia> logrange(10, 4000, length=3)
    3-element Base.LogRange{Float64, Base.TwicePrecision{Float64}}:
     10.0, 200.0, 4000.0

    julia> ans[2] ≈ sqrt(10 * 4000)  # middle element is the geometric mean
    true

    julia> range(10, 40, length=3)[2] ≈ (10 + 40)/2  # arithmetic mean
    true

    julia> logrange(1f0, 32f0, 11)
    11-element Base.LogRange{Float32, Float64}:
     1.0, 1.41421, 2.0, 2.82843, 4.0, 5.65685, 8.0, 11.3137, 16.0, 22.6274, 32.0

    julia> logrange(1, 1000, length=4) ≈ 10 .^ (0:3)
    true
    ```

    See the [`Compat.LogRange`](@ref Compat.LogRange) type for further details.

    !!! compat "Julia 1.9"
        The version of this struct in Compat.jl does not use `Base.TwicePrecision{Float64}`
        before Julia 1.9, so it sometimes has larger floating-point errors on intermediate points.

    !!! compat "Julia 1.11"
        The printing of Compat.jl's version of the struct is also different,
        less like `LinRange` and more like `Vector`.
    """ logrange

    logrange(start::Real, stop::Real, length::Integer) = LogRange(start, stop, Int(length))
    logrange(start::Real, stop::Real; length::Integer) = logrange(start, stop, length)

    @doc """
        LogRange{T}(start, stop, len) <: AbstractVector{T}

    A range whose elements are spaced logarithmically between `start` and `stop`,
    with spacing controlled by `len`. Returned by [`logrange`](@ref).

    Like [`LinRange`](@ref), the first and last elements will be exactly those
    provided, but intermediate values may have small floating-point errors.
    These are calculated using the logs of the endpoints, which are
    stored on construction, often in higher precision than `T`.

    !!! compat "Julia 1.9"
        The version of this struct in Compat.jl does not use `Base.TwicePrecision{Float64}`
        before Julia 1.9. Therefore it has larger floating-point errors on intermediate
        points than shown below.

    !!! compat "Julia 1.11"
        The printing of Compat.jl's version of the struct is also different,
        less like `LinRange` and more like `Vector`.

    # Examples
    ```
    julia> logrange(1, 4, length=5)
    5-element Base.LogRange{Float64, Base.TwicePrecision{Float64}}:
     1.0, 1.41421, 2.0, 2.82843, 4.0

    julia> Base.LogRange{Float16}(1, 4, 5)
    5-element Base.LogRange{Float16, Float64}:
     1.0, 1.414, 2.0, 2.828, 4.0

    julia> logrange(1e-310, 1e-300, 11)[1:2:end]
    6-element Vector{Float64}:
     1.0e-310
     9.999999999999974e-309
     9.999999999999981e-307
     9.999999999999988e-305
     9.999999999999994e-303
     1.0e-300

    julia> prevfloat(1e-308, 5) == ans[2]
    true
    ```

    Note that integer eltype `T` is not allowed.
    Use for instance `round.(Int, xs)`, or explicit powers of some integer base:

    ```
    julia> xs = logrange(1, 512, 4)
    4-element Base.LogRange{Float64, Base.TwicePrecision{Float64}}:
     1.0, 8.0, 64.0, 512.0

    julia> 2 .^ (0:3:9) |> println
    [1, 8, 64, 512]
    ```
    """ LogRange

    struct LogRange{T<:Real,X} <: AbstractArray{T,1}
        start::T
        stop::T
        len::Int
        extra::Tuple{X,X}
        function LogRange{T}(start::T, stop::T, len::Int) where {T<:Real}
            if T <: Integer
                # LogRange{Int}(1, 512, 4) produces InexactError: Int64(7.999999999999998)
                throw(ArgumentError("LogRange{T} does not support integer types"))
            end
            if iszero(start) || iszero(stop)
                throw(DomainError((start, stop),
                    "LogRange cannot start or stop at zero"))
            elseif start < 0 || stop < 0
                # log would throw, but _log_twice64_unchecked does not
                throw(DomainError((start, stop),
                    "LogRange does not accept negative numbers"))
            elseif !isfinite(start) || !isfinite(stop)
                throw(DomainError((start, stop),
                    "LogRange is only defined for finite start & stop"))
            elseif len < 0
                throw(ArgumentError(string( # LazyString(
                    "LogRange(", start, ", ", stop, ", ", len, "): can't have negative length")))
            elseif len == 1 && start != stop
                throw(ArgumentError(string( # LazyString(
                    "LogRange(", start, ", ", stop, ", ", len, "): endpoints differ, while length is 1")))
            end
            ex = _logrange_extra(start, stop, len)
            new{T,typeof(ex[1])}(start, stop, len, ex)
        end
    end

    function LogRange{T}(start::Real, stop::Real, len::Integer) where {T}
        LogRange{T}(convert(T, start), convert(T, stop), convert(Int, len))
    end
    function LogRange(start::Real, stop::Real, len::Integer)
        T = float(promote_type(typeof(start), typeof(stop)))
        LogRange{T}(convert(T, start), convert(T, stop), convert(Int, len))
    end

    Base.size(r::LogRange) = (r.len,)
    Base.length(r::LogRange) = r.len

    Base.first(r::LogRange) = r.start
    Base.last(r::LogRange) = r.stop

    function _logrange_extra(a::Real, b::Real, len::Int)
        loga = log(1.0 * a)  # widen to at least Float64
        logb = log(1.0 * b)
        (loga/(len-1), logb/(len-1))
    end

    function Base.getindex(r::LogRange{T}, i::Int) where {T}
        @inline
        @boundscheck checkbounds(r, i)
        i == 1 && return r.start
        i == r.len && return r.stop
        # Main path uses Math.exp_impl for TwicePrecision, but is not perfectly
        # accurate, hence the special cases for endpoints above.
        logx = (r.len-i) * r.extra[1] + (i-1) * r.extra[2]
        x = _exp_allowing_twice64(logx)
        return copysign(T(x), r.start)
    end

    function Base.show(io::IO, r::LogRange{T}) where {T}
        print(io, "LogRange{", T, "}(")
        ioc = IOContext(io, :typeinfo => T)
        show(ioc, first(r))
        print(io, ", ")
        show(ioc, last(r))
        print(io, ", ")
        show(io, length(r))
        print(io, ')')
    end

    # Display LogRange like LinRange -- PR widened signature of print_range to allow this
    # function Base.show(io::IO, ::MIME"text/plain", r::LogRange)
    #     isempty(r) && return show(io, r)
    #     summary(io, r)
    #     println(io, ":")
    #     print_range(io, r, " ", ", ", "", " \u2026 ")
    # end

    _exp_allowing_twice64(x::Number) = exp(x)

    _exp_allowing_twice64(x::Base.TwicePrecision{Float64}) = Base.Math.exp_impl(x.hi, x.lo, Val(:ℯ))

    function _log_twice64_unchecked(x::Float64)
        xu = reinterpret(UInt64, x)
        if xu < (UInt64(1)<<52) # x is subnormal
            xu = reinterpret(UInt64, x * 0x1p52) # normalize x
            xu &= ~Base.sign_mask(Float64)
            xu -= UInt64(52) << 52 # mess with the exponent
        end
        Base.TwicePrecision(Base.Math._log_ext(xu)...)
    end

    function _logrange_extra(a::Float64, b::Float64, len::Int)
        loga = _log_twice64_unchecked(a)
        logb = _log_twice64_unchecked(b)
        # The reason not to do linear interpolation on log(a)..log(b) in `getindex` is
        # that division of TwicePrecision is quite slow, so do it once on construction:
        (loga/(len-1), logb/(len-1))
    end
else
    # Ensure that Compat.LogRange is always this struct, not exported from Base
    using Base: LogRange
end

if VERSION < v"1.12.0-DEV.974"  # contrib/commit-name.sh 2635dea

    insertdims(A; dims) = _insertdims(A, dims)

    function _insertdims(A::AbstractArray{T, N}, dims::NTuple{M, Int}) where {T, N, M}
        for i in eachindex(dims)
            1 ≤ dims[i] || throw(ArgumentError("the smallest entry in dims must be ≥ 1"))
            dims[i] ≤ N+M || throw(ArgumentError("the largest entry in dims must be not larger than the dimension of the array and the length of dims added"))
            for j = 1:i-1
                dims[j] == dims[i] && throw(ArgumentError("inserted dims must be unique"))
            end
        end

        # acc is a tuple, where the first entry is the final shape
        # the second entry off acc is a counter for the axes of A
        inds = Base._foldoneto((acc, i) ->
                            i ∈ dims
                                ? ((acc[1]..., Base.OneTo(1)), acc[2])
                                : ((acc[1]..., axes(A, acc[2])), acc[2] + 1),
                            ((), 1), Val(N+M))
        new_shape = inds[1]
        return reshape(A, new_shape)
    end

    _insertdims(A::AbstractArray, dim::Integer) = _insertdims(A, (Int(dim),))

    export insertdims
else
    using Base: insertdims, _insertdims
end
                                
# https://github.com/JuliaLang/julia/pull/54653: add Fix
@static if !isdefined(Base, :Fix) # VERSION < v"1.12.0-DEV.981"
    @static if !isdefined(Base, :_stable_typeof)
        _stable_typeof(x) = typeof(x)
        _stable_typeof(::Type{T}) where {T} = Type{T}
    else
        using Base: _stable_typeof
    end

    @doc """
        Fix{N}(f, x)

    A type representing a partially-applied version of a function `f`, with the argument
    `x` fixed at position `N::Int`. In other words, `Fix{3}(f, x)` behaves similarly to
    `(y1, y2, y3...; kws...) -> f(y1, y2, x, y3...; kws...)`.

    !!! note
        When nesting multiple `Fix`, note that the `N` in `Fix{N}` is _relative_ to the current
        available arguments, rather than an absolute ordering on the target function. For example,
        `Fix{1}(Fix{2}(f, 4), 4)` fixes the first and second arg, while `Fix{2}(Fix{1}(f, 4), 4)`
        fixes the first and third arg.

    !!! note
        Note that `Compat.Fix{1}`/`Fix{2}` are not the same as `Base.Fix1`/`Fix2` on Julia
        versions earlier than `1.12.0-DEV.981`. Therefore, if you wish to use this as a way
        to _dispatch_ on `Fix{N}`, you may wish to declare a method for both
        `Compat.Fix{1}`/`Fix{2}` as well as `Base.Fix1`/`Fix2`, conditional on
        a `@static if !isdefined(Base, :Fix); ...; end`.
    """ Fix

    struct Fix{N,F,T} <: Function
        f::F
        x::T

        function Fix{N}(f::F, x) where {N,F}
            if !(N isa Int)
                throw(ArgumentError("expected type parameter in `Fix` to be `Int`, but got `$N::$(typeof(N))`"))
            elseif N < 1
                throw(ArgumentError("expected `N` in `Fix{N}` to be integer greater than 0, but got $N"))
            end
            new{N,_stable_typeof(f),_stable_typeof(x)}(f, x)
        end
    end

    function (f::Fix{N})(args::Vararg{Any,M}; kws...) where {N,M}
        M < N-1 && throw(ArgumentError("expected at least $(N-1) arguments to `Fix{$N}`, but got $M"))
        return f.f(args[begin:begin+(N-2)]..., f.x, args[begin+(N-1):end]...; kws...)
    end

    # Special cases for improved constant propagation
    (f::Fix{1})(arg; kws...) = f.f(f.x, arg; kws...)
    (f::Fix{2})(arg; kws...) = f.f(arg, f.x; kws...)

    @doc """
    Alias for `Fix{1}`. See [`Fix`](@ref Compat.Fix).
    """ Fix1

    const Fix1{F,T} = Fix{1,F,T}

    @doc """
    Alias for `Fix{2}`. See [`Fix`](@ref Compat.Fix).
    """ Fix2

    const Fix2{F,T} = Fix{2,F,T}
else
    using Base: Fix, Fix1, Fix2
end

include("deprecated.jl")

end # module Compat
