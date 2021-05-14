module Compat

import Dates
using Dates: Period, CompoundPeriod

import LinearAlgebra
using LinearAlgebra: Adjoint, Diagonal, Transpose, UniformScaling, RealHermSymComplexHerm, BLAS

include("compatmacro.jl")

# https://github.com/JuliaLang/julia/pull/29440
if VERSION < v"1.1.0-DEV.389"
    Base.:(:)(I::CartesianIndex{N}, J::CartesianIndex{N}) where N =
        CartesianIndices(map((i,j) -> i:j, Tuple(I), Tuple(J)))
end

# https://github.com/JuliaLang/julia/pull/29442
if VERSION < v"1.1.0-DEV.403"
    Base.oneunit(::CartesianIndex{N}) where {N} = oneunit(CartesianIndex{N})
    Base.oneunit(::Type{CartesianIndex{N}}) where {N} = CartesianIndex(ntuple(x -> 1, Val(N)))
end

# https://github.com/JuliaLang/julia/pull/30268
if VERSION < v"1.1.0-DEV.811"
    Base.get(A::AbstractArray, I::CartesianIndex, default) = get(A, I.I, default)
end

# https://github.com/JuliaLang/julia/pull/29679
if VERSION < v"1.1.0-DEV.472"
    export isnothing
    isnothing(::Any) = false
    isnothing(::Nothing) = true
end

# https://github.com/JuliaLang/julia/pull/29749
if VERSION < v"1.1.0-DEV.792"
    export eachrow, eachcol, eachslice
    eachrow(A::AbstractVecOrMat) = (view(A, i, :) for i in axes(A, 1))
    eachcol(A::AbstractVecOrMat) = (view(A, :, i) for i in axes(A, 2))
    @inline function eachslice(A::AbstractArray; dims)
        length(dims) == 1 || throw(ArgumentError("only single dimensions are supported"))
        dim = first(dims)
        dim <= ndims(A) || throw(DimensionMismatch("A doesn't have $dim dimensions"))
        idx1, idx2 = ntuple(d->(:), dim-1), ntuple(d->(:), ndims(A)-dim)
        return (view(A, idx1..., i, idx2...) for i in axes(A, dim))
    end
end

function rangeargcheck(;step=nothing, length=nothing, kwargs...)
    if step===nothing && length===nothing
        throw(ArgumentError("At least one of `length` or `step` must be specified"))
    end
end

if VERSION < v"1.1.0-DEV.506"
    function Base.range(start, stop; kwargs...)
        rangeargcheck(;kwargs...)
        range(start; stop=stop, kwargs...)
    end
end

# https://github.com/JuliaLang/julia/pull/30496
if VERSION < v"1.2.0-DEV.272"
    Base.@pure hasfield(::Type{T}, name::Symbol) where T =
        Base.fieldindex(T, name, false) > 0
    export hasfield
    hasproperty(x, s::Symbol) = s in propertynames(x)
    export hasproperty
end

if VERSION < v"1.3.0-DEV.349"
    Base.findfirst(ch::AbstractChar, string::AbstractString) = findfirst(==(ch), string)
    Base.findnext(ch::AbstractChar, string::AbstractString, ind::Integer) =
        findnext(==(ch), string, ind)
    Base.findlast(ch::AbstractChar, string::AbstractString) = findlast(==(ch), string)
    Base.findprev(ch::AbstractChar, string::AbstractString, ind::Integer) =
        findprev(==(ch), string, ind)
end

# https://github.com/JuliaLang/julia/pull/29259
if VERSION < v"1.1.0-DEV.594"
    Base.merge(a::NamedTuple, b::NamedTuple, cs::NamedTuple...) = merge(merge(a, b), cs...)
    Base.merge(a::NamedTuple) = a
end

# https://github.com/JuliaLang/julia/pull/33129
if VERSION < v"1.4.0-DEV.142"
    export only

    Base.@propagate_inbounds function only(x)
        i = iterate(x)
        @boundscheck if i === nothing
            throw(ArgumentError("Collection is empty, must contain exactly 1 element"))
        end
        (ret, state) = i
        @boundscheck if iterate(x, state) !== nothing
            throw(ArgumentError("Collection has multiple elements, must contain exactly 1 element"))
        end
        return ret
    end

    # Collections of known size
    only(x::Ref) = x[]
    only(x::Number) = x
    only(x::Char) = x
    only(x::Tuple{Any}) = x[1]
    only(x::Tuple) = throw(
        ArgumentError("Tuple contains $(length(x)) elements, must contain exactly 1 element")
    )
    only(a::AbstractArray{<:Any, 0}) = @inbounds return a[]
    only(x::NamedTuple{<:Any, <:Tuple{Any}}) = first(x)
    only(x::NamedTuple) = throw(
        ArgumentError("NamedTuple contains $(length(x)) elements, must contain exactly 1 element")
    )
end

# https://github.com/JuliaLang/julia/pull/32628
if VERSION < v"1.3.0-alpha.8"
    Base.mod(i::Integer, r::Base.OneTo) = mod1(i, last(r))
    Base.mod(i::Integer, r::AbstractUnitRange{<:Integer}) = mod(i-first(r), length(r)) + first(r)
end

# https://github.com/JuliaLang/julia/pull/32739
# This omits special methods for more exotic matrix types, Triangular and worse.
if VERSION < v"1.4.0-DEV.92" # 2425ae760fb5151c5c7dd0554e87c5fc9e24de73

    # stdlib/LinearAlgebra/src/generic.jl
    LinearAlgebra.dot(x, A, y) = LinearAlgebra.dot(x, A*y) # generic fallback

    function LinearAlgebra.dot(x::AbstractVector, A::AbstractMatrix, y::AbstractVector)
        (axes(x)..., axes(y)...) == axes(A) || throw(DimensionMismatch())
        T = typeof(LinearAlgebra.dot(first(x), first(A), first(y)))
        s = zero(T)
        i₁ = first(eachindex(x))
        x₁ = first(x)
        @inbounds for j in eachindex(y)
            yj = y[j]
            if !iszero(yj)
                temp = zero(adjoint(A[i₁,j]) * x₁)
                @simd for i in eachindex(x)
                    temp += adjoint(A[i,j]) * x[i]
                end
                s += LinearAlgebra.dot(temp, yj)
            end
        end
        return s
    end
    LinearAlgebra.dot(x::AbstractVector, adjA::Adjoint, y::AbstractVector) =
         adjoint(LinearAlgebra.dot(y, adjA.parent, x))
    LinearAlgebra.dot(x::AbstractVector, transA::Transpose{<:Real}, y::AbstractVector) =
        adjoint(LinearAlgebra.dot(y, transA.parent, x))

    # stdlib/LinearAlgebra/src/diagonal.jl
    function LinearAlgebra.dot(x::AbstractVector, D::Diagonal, y::AbstractVector)
        mapreduce(t -> LinearAlgebra.dot(t[1], t[2], t[3]), +, zip(x, D.diag, y))
    end

    # stdlib/LinearAlgebra/src/symmetric.jl
    function LinearAlgebra.dot(x::AbstractVector, A::RealHermSymComplexHerm, y::AbstractVector)
        require_one_based_indexing(x, y)
        (length(x) == length(y) == size(A, 1)) || throw(DimensionMismatch())
        data = A.data
        r = zero(eltype(x)) * zero(eltype(A)) * zero(eltype(y))
        if A.uplo == 'U'
            @inbounds for j = 1:length(y)
                r += LinearAlgebra.dot(x[j], real(data[j,j]), y[j])
                @simd for i = 1:j-1
                    Aij = data[i,j]
                    r += LinearAlgebra.dot(x[i], Aij, y[j]) +
                        LinearAlgebra.dot(x[j], adjoint(Aij), y[i])
                end
            end
        else # A.uplo == 'L'
            @inbounds for j = 1:length(y)
                r += LinearAlgebra.dot(x[j], real(data[j,j]), y[j])
                @simd for i = j+1:length(y)
                    Aij = data[i,j]
                    r += LinearAlgebra.dot(x[i], Aij, y[j]) +
                        LinearAlgebra.dot(x[j], adjoint(Aij), y[i])
                end
            end
        end
        return r
    end

    # stdlib/LinearAlgebra/src/uniformscaling.jl
    LinearAlgebra.dot(x::AbstractVector, J::UniformScaling, y::AbstractVector) =
        LinearAlgebra.dot(x, J.λ, y)
    LinearAlgebra.dot(x::AbstractVector, a::Number, y::AbstractVector) =
        sum(t -> LinearAlgebra.dot(t[1], a, t[2]), zip(x, y))
    LinearAlgebra.dot(x::AbstractVector, a::Union{Real,Complex}, y::AbstractVector) =
        a*LinearAlgebra.dot(x, y)
end

# https://github.com/JuliaLang/julia/pull/30630
if VERSION < v"1.2.0-DEV.125" # 1da48c2e4028c1514ed45688be727efbef1db884
    require_one_based_indexing(A...) = !Base.has_offset_axes(A...) || throw(ArgumentError(
        "offset arrays are not supported but got an array with index other than 1"))
# At present this is only used in Compat inside the above dot(x,A,y) functions, #32739
elseif VERSION < v"1.4.0-DEV.92"
    using Base: require_one_based_indexing
end

# https://github.com/JuliaLang/julia/pull/33568
if VERSION < v"1.4.0-DEV.329"
    Base.:∘(f, g, h...) = ∘(f ∘ g, h...)
end
# https://github.com/JuliaLang/julia/pull/34251
if VERSION < v"1.5.0-DEV.56"
    Base.:∘(f) = f
end

# https://github.com/JuliaLang/julia/pull/33128
if VERSION < v"1.4.0-DEV.397"
    export pkgdir
    function pkgdir(m::Module)
        rootmodule = Base.moduleroot(m)
        path = pathof(rootmodule)
        path === nothing && return nothing
        return dirname(dirname(path))
    end
end

# https://github.com/JuliaLang/julia/pull/33736/
if VERSION < v"1.4.0-DEV.493"
    Base.Order.ReverseOrdering() = Base.Order.ReverseOrdering(Base.Order.Forward)
end

# https://github.com/JuliaLang/julia/pull/32968
if VERSION < v"1.4.0-DEV.551"
    Base.filter(f, xs::Tuple) = Base.afoldl((ys, x) -> f(x) ? (ys..., x) : ys, (), xs...)
    Base.filter(f, t::Base.Any16) = Tuple(filter(f, collect(t)))
end

# https://github.com/JuliaLang/julia/pull/34652
if VERSION < v"1.5.0-DEV.247"
    export ismutable
    ismutable(@nospecialize(x)) = (Base.@_pure_meta; typeof(x).mutable)
end

# https://github.com/JuliaLang/julia/pull/28761
export uuid5
if VERSION < v"1.1.0-DEV.326"
    import SHA
    import UUIDs: UUID
    function uuid5(ns::UUID, name::String)
        nsbytes = zeros(UInt8, 16)
        nsv = ns.value
        for idx in Base.OneTo(16)
            nsbytes[idx] = nsv >> 120
            nsv = nsv << 8
        end
        hash_result = SHA.sha1(append!(nsbytes, convert(Vector{UInt8}, codeunits(unescape_string(name)))))
        # set version number to 5
        hash_result[7] = (hash_result[7] & 0x0F) | (0x50)
        hash_result[9] = (hash_result[9] & 0x3F) | (0x80)
        v = zero(UInt128)
        #use only the first 16 bytes of the SHA1 hash
        for idx in Base.OneTo(16)
            v = (v << 0x08) | hash_result[idx]
        end
        return UUID(v)
    end
else
    using UUIDs: uuid5
end

# https://github.com/JuliaLang/julia/pull/34773
if VERSION < v"1.5.0-DEV.301"
    Base.zero(::AbstractIrrational) = false
    Base.zero(::Type{<:AbstractIrrational}) = false

    Base.one(::AbstractIrrational) = true
    Base.one(::Type{<:AbstractIrrational}) = true
end

# https://github.com/JuliaLang/julia/pull/32753
if VERSION < v"1.4.0-DEV.513"
    function evalpoly(x, p::Tuple)
        if @generated
            N = length(p.parameters)
            ex = :(p[end])
            for i in N-1:-1:1
                ex = :(muladd(x, $ex, p[$i]))
            end
            ex
        else
            _evalpoly(x, p)
        end
    end

    evalpoly(x, p::AbstractVector) = _evalpoly(x, p)

    function _evalpoly(x, p)
        N = length(p)
        ex = p[end]
        for i in N-1:-1:1
            ex = muladd(x, ex, p[i])
        end
        ex
    end

    function evalpoly(z::Complex, p::Tuple)
        if @generated
            N = length(p.parameters)
            a = :(p[end])
            b = :(p[end-1])
            as = []
            for i in N-2:-1:1
                ai = Symbol("a", i)
                push!(as, :($ai = $a))
                a = :(muladd(r, $ai, $b))
                b = :(muladd(-s, $ai, p[$i]))
            end
            ai = :a0
            push!(as, :($ai = $a))
            C = Expr(:block,
            :(x = real(z)),
            :(y = imag(z)),
            :(r = x + x),
            :(s = muladd(x, x, y*y)),
            as...,
            :(muladd($ai, z, $b)))
        else
            _evalpoly(z, p)
        end
    end
    evalpoly(z::Complex, p::Tuple{<:Any}) = p[1]

    evalpoly(z::Complex, p::AbstractVector) = _evalpoly(z, p)

    function _evalpoly(z::Complex, p)
        length(p) == 1 && return p[1]
        N = length(p)
        a = p[end]
        b = p[end-1]

        x = real(z)
        y = imag(z)
        r = 2x
        s = muladd(x, x, y*y)
        for i in N-2:-1:1
            ai = a
            a = muladd(r, ai, b)
            b = muladd(-s, ai, p[i])
        end
        ai = a
        muladd(ai, z, b)
    end
    export evalpoly
end

# https://github.com/JuliaLang/julia/pull/35304
if VERSION < v"1.5.0-DEV.574"
    Base.similar(A::PermutedDimsArray, T::Type, dims::Base.Dims) = similar(parent(A), T, dims)
end

# https://github.com/JuliaLang/julia/pull/34548
if VERSION < v"1.5.0-DEV.314"
    macro NamedTuple(ex)
        Meta.isexpr(ex, :braces) || Meta.isexpr(ex, :block) ||
            throw(ArgumentError("@NamedTuple expects {...} or begin...end"))
        decls = filter(e -> !(e isa LineNumberNode), ex.args)
        all(e -> e isa Symbol || Meta.isexpr(e, :(::)), decls) ||
            throw(ArgumentError("@NamedTuple must contain a sequence of name or name::type expressions"))
        vars = [QuoteNode(e isa Symbol ? e : e.args[1]) for e in decls]
        types = [esc(e isa Symbol ? :Any : e.args[2]) for e in decls]
        return :(NamedTuple{($(vars...),), Tuple{$(types...)}})
    end

    export @NamedTuple
end

# https://github.com/JuliaLang/julia/pull/34296
if VERSION < v"1.5.0-DEV.182"
    export mergewith, mergewith!
    _asfunction(f::Function) = f
    _asfunction(f) = (args...) -> f(args...)
    mergewith(f, dicts...) = merge(_asfunction(f), dicts...)
    mergewith!(f, dicts...) = merge!(_asfunction(f), dicts...)
    mergewith(f) = (dicts...) -> mergewith(f, dicts...)
    mergewith!(f) = (dicts...) -> mergewith!(f, dicts...)
end

# https://github.com/JuliaLang/julia/pull/32003
if VERSION < v"1.4.0-DEV.29"
    hasfastin(::Type) = false
    hasfastin(::Union{Type{<:AbstractSet},Type{<:AbstractDict},Type{<:AbstractRange}}) = true
    hasfastin(x) = hasfastin(typeof(x))
else
    const hasfastin = Base.hasfastin
end

# https://github.com/JuliaLang/julia/pull/34427
if VERSION < v"1.5.0-DEV.124"
    const FASTIN_SET_THRESHOLD = 70

    function isdisjoint(l, r)
        function _isdisjoint(l, r)
            hasfastin(r) && return !any(in(r), l)
            hasfastin(l) && return !any(in(l), r)
            Base.haslength(r) && length(r) < FASTIN_SET_THRESHOLD &&
                return !any(in(r), l)
            return !any(in(Set(r)), l)
        end
        if Base.haslength(l) && Base.haslength(r) && length(r) < length(l)
            return _isdisjoint(r, l)
        end
        _isdisjoint(l, r)
    end

    export isdisjoint
end

# https://github.com/JuliaLang/julia/pull/35577
if VERSION < v"1.5.0-DEV.681"
    Base.union(r::Base.OneTo, s::Base.OneTo) = Base.OneTo(max(r.stop,s.stop))
end

# https://github.com/JuliaLang/julia/pull/35929
# and also https://github.com/JuliaLang/julia/pull/29135 -> Julia 1.5
if VERSION < v"1.5.0-rc1.13" || v"1.6.0-" < VERSION < v"1.6.0-DEV.323"

    # Compat.stride not Base.stride, so as not to overwrite the method, and not to create ambiguities:
    function stride(A::AbstractArray, k::Integer)
        st = strides(A)
        k ≤ ndims(A) && return st[k]
        return sum(st .* size(A))
    end
    stride(A,k) = Base.stride(A,k) # Fall-through for other methods.

    # These were first defined for Adjoint{...,StridedVector} etc in #29135
    Base.strides(A::Adjoint{<:Real, <:AbstractVector}) = (stride(A.parent, 2), stride(A.parent, 1))
    Base.strides(A::Transpose{<:Any, <:AbstractVector}) = (stride(A.parent, 2), stride(A.parent, 1))
    Base.strides(A::Adjoint{<:Real, <:AbstractMatrix}) = reverse(strides(A.parent))
    Base.strides(A::Transpose{<:Any, <:AbstractMatrix}) = reverse(strides(A.parent))
    Base.unsafe_convert(::Type{Ptr{T}}, A::Adjoint{<:Real, <:AbstractVecOrMat}) where {T} = Base.unsafe_convert(Ptr{T}, A.parent)
    Base.unsafe_convert(::Type{Ptr{T}}, A::Transpose{<:Any, <:AbstractVecOrMat}) where {T} = Base.unsafe_convert(Ptr{T}, A.parent)

    Base.elsize(::Type{<:Adjoint{<:Real, P}}) where {P<:AbstractVecOrMat} = Base.elsize(P)
    Base.elsize(::Type{<:Transpose{<:Any, P}}) where {P<:AbstractVecOrMat} = Base.elsize(P)

end

# https://github.com/JuliaLang/julia/pull/27516
if VERSION < v"1.2.0-DEV.77"
    import Test: @inferred
    using Core.Compiler: typesubtract

    macro inferred(allow, ex)
        _inferred(ex, __module__, allow)
    end

    function _inferred(ex, mod, allow = :(Union{}))
        if Meta.isexpr(ex, :ref)
            ex = Expr(:call, :getindex, ex.args...)
        end
        Meta.isexpr(ex, :call)|| error("@inferred requires a call expression")
        farg = ex.args[1]
        if isa(farg, Symbol) && first(string(farg)) == '.'
            farg = Symbol(string(farg)[2:end])
            ex = Expr(:call, GlobalRef(Base.Test, :_materialize_broadcasted),
                farg, ex.args[2:end]...)
        end
        Base.remove_linenums!(quote
            let
                allow = $(esc(allow))
                allow isa Type || throw(ArgumentError("@inferred requires a type as second argument"))
                $(if any(a->(Meta.isexpr(a, :kw) || Meta.isexpr(a, :parameters)), ex.args)
                    # Has keywords
                    args = gensym()
                    kwargs = gensym()
                    quote
                        $(esc(args)), $(esc(kwargs)), result = $(esc(Expr(:call, _args_and_call, ex.args[2:end]..., ex.args[1])))
                        inftypes = $(gen_call_with_extracted_types(mod, Base.return_types, :($(ex.args[1])($(args)...; $(kwargs)...))))
                    end
                else
                    # No keywords
                    quote
                        args = ($([esc(ex.args[i]) for i = 2:length(ex.args)]...),)
                        result = $(esc(ex.args[1]))(args...)
                        inftypes = Base.return_types($(esc(ex.args[1])), Base.typesof(args...))
                    end
                end)
                @assert length(inftypes) == 1
                rettype = result isa Type ? Type{result} : typeof(result)
                rettype <: allow || rettype == typesubtract(inftypes[1], allow) || error("return type $rettype does not match inferred return type $(inftypes[1])")
                result
            end
        end)
    end
    #export @inferred
end

# https://github.com/JuliaLang/julia/pull/36360
if VERSION < v"1.6.0-DEV.322" # b8110f8d1ec6349bee77efb5022621fdf50bd4a5

    function guess_vendor()
        # like determine_vendor, but guesses blas in some cases
        # where determine_vendor returns :unknown
        ret = BLAS.vendor()
        if Base.Sys.isapple() && (ret == :unknown)
            ret = :osxblas
        end
        ret
    end

    """
        Compat.set_num_threads(n)

    Set the number of threads the BLAS library should use.

    Also accepts `nothing`, in which case julia tries to guess the default number of threads.
    Passing `nothing` is discouraged and mainly exists because,
    on exotic variants of BLAS, `nothing` may be returned by `get_num_threads()`.
    Thus the following pattern may fail to set the number of threads, but will not error:
    ```julia
    old = get_num_threads()
    set_num_threads(1)
    @threads for i in 1:10
        # single-threaded BLAS calls
    end
    set_num_threads(old)
    ```
    """
    set_num_threads(n)::Nothing = _set_num_threads(n)

    function _set_num_threads(n::Integer; _blas = guess_vendor())
        if _blas === :openblas || _blas == :openblas64
            return ccall((BLAS.@blasfunc(openblas_set_num_threads), BLAS.libblas), Cvoid, (Cint,), n)
        elseif _blas === :mkl
           # MKL may let us set the number of threads in several ways
            return ccall((:MKL_Set_Num_Threads, BLAS.libblas), Cvoid, (Cint,), n)
            elseif _blas === :osxblas
            # OSX BLAS looks at an environment variable
            ENV["VECLIB_MAXIMUM_THREADS"] = n
        else
            @assert _blas === :unknown
            @warn "Failed to set number of BLAS threads." maxlog=1
        end
        return nothing
    end
    _tryparse_env_int(key) = tryparse(Int, get(ENV, key, ""))

    function _set_num_threads(::Nothing; _blas = guess_vendor())
        n = something(
            _tryparse_env_int("OPENBLAS_NUM_THREADS"),
            _tryparse_env_int("OMP_NUM_THREADS"),
            max(1, Base.Sys.CPU_THREADS ÷ 2),
        )
        _set_num_threads(n; _blas = _blas)
    end

    """
        Compat.get_num_threads()

    Get the number of threads the BLAS library is using.

    On exotic variants of `BLAS` this function can fail,
    which is indicated by returning `nothing`.

    In Julia 1.6 this is `LinearAlgebra.BLAS.get_num_threads()`
    """
    get_num_threads(;_blas=guess_vendor())::Union{Int, Nothing} = _get_num_threads()

    function _get_num_threads(; _blas = guess_vendor())::Union{Int, Nothing}
        if _blas === :openblas || _blas === :openblas64
            return Int(ccall((BLAS.@blasfunc(openblas_get_num_threads), BLAS.libblas), Cint, ()))
        elseif _blas === :mkl
            return Int(ccall((:mkl_get_max_threads, BLAS.libblas), Cint, ()))
        elseif _blas === :osxblas
            key = "VECLIB_MAXIMUM_THREADS"
            nt = _tryparse_env_int(key)
            if nt === nothing
                @warn "Failed to read environment variable $key" maxlog=1
            else
                return nt
            end
        else
            @assert _blas === :unknown
        end
        @warn "Could not get number of BLAS threads. Returning `nothing` instead." maxlog=1
        return nothing
    end

else
    # Ensure that these can still be accessed as Compat.get_num_threads() etc:
    import LinearAlgebra.BLAS: set_num_threads, get_num_threads
end

# https://github.com/JuliaLang/julia/pull/30915
if VERSION < v"1.2.0-DEV.257" # e7e726b3df1991e1306ef0c566d363c0a83b2dea
    Base.:(!=)(x) = Base.Fix2(!=, x)
    Base.:(>=)(x) = Base.Fix2(>=, x)
    Base.:(<=)(x) = Base.Fix2(<=, x)
    Base.:(>)(x) = Base.Fix2(>, x)
    Base.:(<)(x) = Base.Fix2(<, x)
end

# https://github.com/JuliaLang/julia/pull/35132
if VERSION < v"1.5.0-DEV.639" # cc6e121386758dff6ba7911770e48dfd59520199
    export contains
    contains(haystack::AbstractString, needle) = occursin(needle, haystack)
    contains(needle) = Base.Fix2(contains, needle)
end

# https://github.com/JuliaLang/julia/pull/35052
if VERSION < v"1.5.0-DEV.438" # 0a43c0f1d21ce9c647c49111d93927369cd20f85
    Base.endswith(s) = Base.Fix2(endswith, s)
    Base.startswith(s) = Base.Fix2(startswith, s)
end

# https://github.com/JuliaLang/julia/pull/37517
if VERSION < v"1.6.0-DEV.1037"
    export ComposedFunction
    # https://github.com/JuliaLang/julia/pull/35980
    if VERSION < v"1.6.0-DEV.85"
        const ComposedFunction = let h = identity ∘ convert
            Base.typename(typeof(h)).wrapper
        end
        @eval ComposedFunction{F,G}(f, g) where {F,G} =
            $(Expr(:new, :(ComposedFunction{F,G}), :f, :g))
        ComposedFunction(f, g) = ComposedFunction{Core.Typeof(f),Core.Typeof(g)}(f, g)
    else
        using Base: ComposedFunction
    end
    function Base.getproperty(c::ComposedFunction, p::Symbol)
        if p === :f
            return getfield(c, :f)
        elseif p === :g
            return getfield(c, :g)
        elseif p === :outer
            return getfield(c, :f)
        elseif p === :inner
            return getfield(c, :g)
        end
        error("type ComposedFunction has no property ", p)
    end
    Base.propertynames(c::ComposedFunction) = (:f, :g, :outer, :inner)
else
    using Base: ComposedFunction
end

# https://github.com/JuliaLang/julia/pull/37244
if VERSION < v"1.6.0-DEV.873" # 18198b1bf85125de6cec266eac404d31ccc2e65c
    export addenv
    function addenv(cmd::Cmd, env::Dict)
        new_env = Dict{String,String}()
        if cmd.env !== nothing
            for (k, v) in split.(cmd.env, "=")
                new_env[string(k)::String] = string(v)::String
            end
        end
        for (k, v) in env
            new_env[string(k)::String] = string(v)::String
        end
        return setenv(cmd, new_env)
    end

    function addenv(cmd::Cmd, pairs::Pair{<:AbstractString}...)
        return addenv(cmd, Dict(k => v for (k, v) in pairs))
    end

    function addenv(cmd::Cmd, env::Vector{<:AbstractString})
        return addenv(cmd, Dict(k => v for (k, v) in split.(env, "=")))
    end
end


# https://github.com/JuliaLang/julia/pull/37559
if VERSION < v"1.6.0-DEV.1083"
    """
        reinterpret(reshape, T, A::AbstractArray{S}) -> B

    Change the type-interpretation of `A` while consuming or adding a "channel dimension."

    If `sizeof(T) = n*sizeof(S)` for `n>1`, `A`'s first dimension must be
    of size `n` and `B` lacks `A`'s first dimension. Conversely, if `sizeof(S) = n*sizeof(T)` for `n>1`,
    `B` gets a new first dimension of size `n`. The dimensionality is unchanged if `sizeof(T) == sizeof(S)`.

    # Examples

    ```
    julia> A = [1 2; 3 4]
    2×2 Matrix{$Int}:
     1  2
     3  4

    julia> reinterpret(reshape, Complex{Int}, A)    # the result is a vector
    2-element reinterpret(reshape, Complex{$Int}, ::Matrix{$Int}):
     1 + 3im
     2 + 4im

    julia> a = [(1,2,3), (4,5,6)]
    2-element Vector{Tuple{$Int, $Int, $Int}}:
     (1, 2, 3)
     (4, 5, 6)

    julia> reinterpret(reshape, Int, a)             # the result is a matrix
    3×2 reinterpret(reshape, $Int, ::Vector{Tuple{$Int, $Int, $Int}}):
     1  4
     2  5
     3  6
    ```
    """
    function Base.reinterpret(::typeof(reshape), ::Type{T}, a::A) where {T,S,A<:AbstractArray{S}}
        isbitstype(T) || throwbits(S, T, T)
        isbitstype(S) || throwbits(S, T, S)
        if sizeof(S) == sizeof(T)
            N = ndims(a)
        elseif sizeof(S) > sizeof(T)
            rem(sizeof(S), sizeof(T)) == 0 || throwintmult(S, T)
            N = ndims(a) + 1
        else
            rem(sizeof(T), sizeof(S)) == 0 || throwintmult(S, T)
            N = ndims(a) - 1
            N > -1 || throwsize0(S, T, "larger")
            axes(a, 1) == Base.OneTo(sizeof(T) ÷ sizeof(S)) || throwsize1(a, T)
        end
        paxs = axes(a)
        new_axes = if sizeof(S) > sizeof(T)
            (Base.OneTo(div(sizeof(S), sizeof(T))), paxs...)
        elseif sizeof(S) < sizeof(T)
            Base.tail(paxs)
        else
            paxs
        end
        reshape(reinterpret(T, vec(a)), new_axes)
    end

    @noinline function throwintmult(S::Type, T::Type)
        throw(ArgumentError("`reinterpret(reshape, T, a)` requires that one of `sizeof(T)` (got $(sizeof(T))) and `sizeof(eltype(a))` (got $(sizeof(S))) be an integer multiple of the other"))
    end
    @noinline function throwsize1(a::AbstractArray, T::Type)
        throw(ArgumentError("`reinterpret(reshape, $T, a)` where `eltype(a)` is $(eltype(a)) requires that `axes(a, 1)` (got $(axes(a, 1))) be equal to 1:$(sizeof(T) ÷ sizeof(eltype(a))) (from the ratio of element sizes)"))
    end
    @noinline function throwbits(S::Type, T::Type, U::Type)
        throw(ArgumentError("cannot reinterpret `$(S)` as `$(T)`, type `$(U)` is not a bits type"))
    end
    @noinline function throwsize0(S::Type, T::Type, msg)
        throw(ArgumentError("cannot reinterpret a zero-dimensional `$(S)` array to `$(T)` which is of a $msg size"))
    end
end

if VERSION < v"1.3.0-alpha.115"
    # https://github.com/JuliaLang/julia/pull/29634
    # Note this is much less performant than real 5-arg mul!, but is provided so old versions of julia don't error at least

    function _mul!(C, A, B, alpha, beta)
        Y = similar(C)
        LinearAlgebra.mul!(Y, A, B)
        C .= Y .* alpha .+ C .* beta
        return C
    end

    # all combination of Number and AbstractArray for A and B except both being Number
    function LinearAlgebra.mul!(C::AbstractArray, A::Number, B::AbstractArray, alpha::Number, beta::Number)
        return _mul!(C, A, B, alpha, beta)
    end
    function LinearAlgebra.mul!(C::AbstractArray, A::AbstractArray, B::Number, alpha::Number, beta::Number)
        return _mul!(C, A, B, alpha, beta)
    end
    function LinearAlgebra.mul!(C::AbstractArray, A::AbstractArray, B::AbstractArray, alpha::Number, beta::Number)
        return _mul!(C, A, B, alpha, beta)
    end
end

# https://github.com/JuliaLang/julia/pull/35243
if VERSION < v"1.6.0-DEV.15"
    _replace_filename(@nospecialize(x), filename, line_offset=0) = x
    function _replace_filename(x::LineNumberNode, filename, line_offset=0)
        return LineNumberNode(x.line + line_offset, filename)
    end
    function _replace_filename(ex::Expr, filename, line_offset=0)
        return Expr(
            ex.head,
            Any[_replace_filename(i, filename, line_offset) for i in ex.args]...,
        )
    end

    function parseatom(text::AbstractString, pos::Integer; filename="none")
        ex, i = Meta.parse(text, pos, greedy=false)
        return _replace_filename(ex, Symbol(filename)), i
    end

    function _skip_newlines(text, line, i)
        while i <= lastindex(text) && isspace(text[i])
            line += text[i] == '\n'
            i = nextind(text, i)
        end
        return line, i
    end

    function parseall(text::AbstractString; filename="none")
        filename = Symbol(filename)
        ex = Expr(:toplevel)
        line, prev_i = _skip_newlines(text, 1, firstindex(text))
        ex_n, i = Meta.parse(text, prev_i)
        while ex_n !== nothing
            push!(ex.args, LineNumberNode(line, filename))
            push!(ex.args, _replace_filename(ex_n, filename, line-1))
            line += count(==('\n'), SubString(text, prev_i:prevind(text, i)))
            line, prev_i = _skip_newlines(text, line, i)
            ex_n, i = Meta.parse(text, prev_i)
        end
        return ex
    end
else
    using .Meta: parseatom, parseall
end

# https://github.com/JuliaLang/julia/pull/37391
if VERSION < v"1.6.0-DEV.820"
    Dates.canonicalize(p::Period) = Dates.canonicalize(CompoundPeriod(p))
end

# https://github.com/JuliaLang/julia/pull/35816
if VERSION < v"1.6.0-DEV.292" # 6cd329c371c1db3d9876bc337e82e274e50420e8
    export sincospi
    sincospi(x) = (sinpi(x), cospi(x))
end

# https://github.com/JuliaLang/julia/pull/38449
if VERSION < v"1.6.0-DEV.1591" # 96d59f957e4c0413e2876592072c0f08a7482cf2
    export cispi
    cispi(theta::Real) = Complex(reverse(sincospi(theta))...)
    function cispi(z::Complex)
        sipi, copi = sincospi(z)
        return complex(real(copi) - imag(sipi), imag(copi) + real(sipi))
    end
end

# https://github.com/JuliaLang/julia/pull/29790
if VERSION < v"1.2.0-DEV.246"
    using Base.PCRE

    function Base.startswith(s::AbstractString, r::Regex)
        Base.compile(r)
        return PCRE.exec(
            r.regex, String(s), 0, r.match_options | PCRE.ANCHORED, r.match_data
        )
    end

    function Base.startswith(s::SubString, r::Regex)
        Base.compile(r)
        return PCRE.exec(r.regex, s, 0, r.match_options | PCRE.ANCHORED, r.match_data)
    end

    function Base.endswith(s::AbstractString, r::Regex)
        Base.compile(r)
        return PCRE.exec(
            r.regex, String(s), 0, r.match_options | PCRE.ENDANCHORED, r.match_data
        )
    end

    function Base.endswith(s::SubString, r::Regex)
        Base.compile(r)
        return PCRE.exec(r.regex, s, 0, r.match_options | PCRE.ENDANCHORED, r.match_data)
    end
end

if VERSION < v"1.7.0-DEV.119"
    # Part of https://github.com/JuliaLang/julia/pull/35316
    isunordered(x) = false
    isunordered(x::AbstractFloat) = isnan(x)
    isunordered(x::Missing) = true

    isgreater(x, y) = isunordered(x) || isunordered(y) ? isless(x, y) : isless(y, x)

    Base.findmax(f, domain) = mapfoldl(x -> (f(x), x), _rf_findmax, domain)
    _rf_findmax((fm, m), (fx, x)) = isless(fm, fx) ? (fx, x) : (fm, m)

    Base.findmin(f, domain) = mapfoldl(x -> (f(x), x), _rf_findmin, domain)
    _rf_findmin((fm, m), (fx, x)) = isgreater(fm, fx) ? (fx, x) : (fm, m)

    Base.argmax(f, domain) = findmax(f, domain)[2]
    Base.argmin(f, domain) = findmin(f, domain)[2]
end

# Part of: https://github.com/JuliaLang/julia/pull/36018
if VERSION < v"1.6.0-DEV.749"
    import UUIDs: UUID
    UUID(u::UUID) = u
end

# https://github.com/JuliaLang/julia/pull/36199
if VERSION < v"1.6.0-DEV.196"
    using UUIDs: UUID
    Base.parse(::Type{UUID}, s::AbstractString) = UUID(s)
end

# https://github.com/JuliaLang/julia/pull/37454
if VERSION < v"1.6.0-DEV.877"
    Base.NamedTuple(itr) = (; itr...)
end

# https://github.com/JuliaLang/julia/pull/40729
if VERSION < v"1.7.0-DEV.1088"
    macro something(args...)
        expr = :(nothing)
        for arg in reverse(args)
            expr = :((val = $arg) !== nothing ? val : $expr)
        end
        return esc(:(something(let val; $expr; end)))
    end

    macro coalesce(args...)
        expr = :(missing)
        for arg in reverse(args)
            expr = :((val = $arg) !== missing ? val : $expr)
        end
        return esc(:(let val; $expr; end))
    end

    export @something, @coalesce
end

include("iterators.jl")
include("deprecated.jl")

end # module Compat
