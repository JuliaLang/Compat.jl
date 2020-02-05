module Compat

import LinearAlgebra
using LinearAlgebra: Adjoint, Diagonal, Transpose, UniformScaling, RealHermSymComplexHerm

include("compatmacro.jl")

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
if VERSION < v"1.5.0-DEV.231"
    export ismutable
    ismutable(@nospecialize(x)) = (Base.@_pure_meta; typeof(x).mutable)
end

include("deprecated.jl")

end # module Compat
