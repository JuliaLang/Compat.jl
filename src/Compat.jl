module Compat

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
        if Sys.isapple() && (ret == :unknown)
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
            max(1, Sys.CPU_THREADS ÷ 2),
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
    using LinearAlgebra
    import BLAS: set_num_threads, get_num_threads
end

include("deprecated.jl")

end # module Compat
