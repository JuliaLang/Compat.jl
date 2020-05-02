# https://github.com/JuliaLang/julia/pull/35150

"""
    hadamard(a, b)
    a ⊙ b

For arrays `a` and `b`, perform elementwise multiplication.
`a` and `b` must have identical `axes`.

`⊙` can be passed as an operator to higher-order functions.

```jldoctest
julia> a = [2, 3]; b = [5, 7];

julia> a ⊙ b
2-element Array{$Int,1}:
 10
 21

julia> a ⊙ [5]
ERROR: DimensionMismatch("Axes of `A` and `B` must match, got (Base.OneTo(2),) and (Base.OneTo(1),)")
[...]
```
"""
function hadamard(A::AbstractArray, B::AbstractArray)
    @noinline throw_dmm(axA, axB) = throw(DimensionMismatch("Axes of `A` and `B` must match, got $axA and $axB"))

    axA, axB = axes(A), axes(B)
    axA == axB || throw_dmm(axA, axB)
    return map(*, A, B)
end
const ⊙ = hadamard

"""
    hadamard!(dest, A, B)

Similar to `hadamard(A, B)` (which can also be written `A ⊙ B`), but stores its results in
the pre-allocated array `dest`.
"""
function hadamard!(dest::AbstractArray, A::AbstractArray, B::AbstractArray)
    @noinline function throw_dmm(axA, axB, axdest)
        throw(DimensionMismatch("`axes(dest) = $axdest` must be equal to `axes(A) = $axA` and `axes(B) = $axB`"))
    end

    axA, axB, axdest = axes(A), axes(B), axes(dest)
    ((axdest == axA) & (axdest == axB)) || throw_dmm(axA, axB, axdest)
    @simd for I in eachindex(dest, A, B)
        @inbounds dest[I] = A[I] * B[I]
    end
    return dest
end

export ⊙, hadamard, hadamard!

"""
    tensor(A, B)
    A ⊗ B

Compute the tensor product of `A` and `B`.
If `C = A ⊗ B`, then `C[i1, ..., im, j1, ..., jn] = A[i1, ... im] * B[j1, ..., jn]`.

```jldoctest
julia> a = [2, 3]; b = [5, 7, 11];

julia> a ⊗ b
2×3 Array{$Int,2}:
 10  14  22
 15  21  33
```

See also: [`kron`](@ref).
"""
tensor(A::AbstractArray, B::AbstractArray) = [a*b for a in A, b in B]
const ⊗ = tensor

const CovectorLike{T} = Union{Adjoint{T,<:AbstractVector},Transpose{T,<:AbstractVector}}
function tensor(u::AbstractArray, v::CovectorLike)
    # If `v` is thought of as a covector, you might want this to be two-dimensional,
    # but thought of as a matrix it should be three-dimensional.
    # The safest is to avoid supporting it at all. See discussion in #35150.
    error("`tensor` is not defined for co-vectors, perhaps you meant `*`?")
end
function tensor(u::CovectorLike, v::AbstractArray)
    error("`tensor` is not defined for co-vectors, perhaps you meant `*`?")
end
function tensor(u::CovectorLike, v::CovectorLike)
    error("`tensor` is not defined for co-vectors, perhaps you meant `*`?")
end

"""
    tensor!(dest, A, B)

Similar to `tensor(A, B)` (which can also be written `A ⊗ B`), but stores its results in
the pre-allocated array `dest`.
"""
function tensor!(dest::AbstractArray, A::AbstractArray, B::AbstractArray)
    @noinline function throw_dmm(axA, axB, axdest)
        throw(DimensionMismatch("`axes(dest) = $axdest` must concatenate `axes(A) = $axA` and `axes(B) = $axB`"))
    end

    axA, axB, axdest = axes(A), axes(B), axes(dest)
    axes(dest) == (axA..., axB...) || throw_dmm(axA, axB, axdest)
    if IndexStyle(dest) === IndexCartesian()
        for IB in CartesianIndices(axB)
            @inbounds b = B[IB]
            @simd for IA in CartesianIndices(axA)
                @inbounds dest[IA,IB] = A[IA]*b
            end
        end
    else
        i = firstindex(dest)
        @inbounds for b in B
            @simd for a in A
                dest[i] = a*b
                i += 1
            end
        end
    end
    return dest
end

export ⊗, tensor, tensor!
