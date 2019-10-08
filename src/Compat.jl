VERSION < v"0.7.0-beta2.199" && __precompile__()

module Compat

# to be deprecated
import Sockets
module TypeUtils
    using Base: parameter_upper_bound, typename
    using Compat: isabstracttype
    const isabstract = isabstracttype
    export isabstract, parameter_upper_bound, typename
end # module TypeUtils
import Base.invokelatest
const macros_have_sourceloc = true
module Sys
    const KERNEL = Base.Sys.KERNEL
    import Base.Sys: isapple, isbsd, islinux, isunix, iswindows
    import Base.Sys: which, isexecutable
    BINDIR = Base.Sys.BINDIR
end
import Base.MathConstants
import Test, SharedArrays, Mmap, DelimitedFiles
import Dates
import Libdl
const AbstractDateTime = Compat.Dates.AbstractDateTime
import Printf
import LinearAlgebra
import SparseArrays
import Random
import Markdown
import REPL
import Serialization
import Statistics
import Base: Fix2
import Base64
const tr = LinearAlgebra.tr
module Unicode
    export graphemes, textwidth, isvalid,
           islower, isupper, isalpha, isdigit, isxdigit, isnumeric, isalnum,
           iscntrl, ispunct, isspace, isprint, isgraph,
           lowercase, uppercase, titlecase, lcfirst, ucfirst
    using Unicode
    import Unicode: isassigned, normalize # not exported from Unicode module due to conflicts
end
import Base: notnothing
const IteratorSize = Base.IteratorSize
const IteratorEltype = Base.IteratorEltype
enable_debug(x::Bool) = x
import Distributed
import Pkg
import InteractiveUtils
import LibGit2
import UUIDs
using LinearAlgebra: qr
using LinearAlgebra: rmul!


include("compatmacro.jl")

# https://github.com/JuliaLang/julia/pull/29679
if VERSION < v"1.1.0-DEV.472"
    export isnothing
    isnothing(::Any) = false
    isnothing(::Nothing) = true
end

# https://github.com/JuliaLang/julia/pull/29749
@static if v"0.7" <= VERSION < v"1.1.0-DEV.792"
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

if VERSION < v"0.7.0-DEV.5278"
    something() = throw(ArgumentError("No value arguments present"))
    something(x::Nothing, y...) = something(y...)
    something(x::Some, y...) = x.value
    something(x::Any, y...) = x
    export something
end

if !isdefined(LinearAlgebra, :opnorm) # julia#27401
    opnorm(A::AbstractMatrix, p::Real=2) = LinearAlgebra.norm(A, p)
    const norm = LinearAlgebra.vecnorm
    const dot = LinearAlgebra.vecdot
else
    const opnorm = LinearAlgebra.opnorm
    const norm = LinearAlgebra.norm
    const dot = LinearAlgebra.dot
end
const ⋅ = dot

if VERSION < v"0.7.0-DEV.2956" # julia#24839
    Base.permutedims(A::AbstractMatrix) = permutedims(A, (2,1))
    Base.permutedims(v::AbstractVector) = reshape(v, (1, length(v)))
end

# https://github.com/JuliaLang/julia/pull/27253
@static if VERSION < v"0.7.0-alpha.44"
    Base.atan(x::Real, y::Real) = atan2(x, y)
end

# https://github.com/JuliaLang/julia/pull/26647
@static if VERSION < v"0.7.0-DEV.4724"
    rsplit(s::AbstractString, splitter; limit::Integer=0, keepempty::Bool=false) =
        Base.rsplit(s, splitter; limit=limit, keep=keepempty)
    split(s::AbstractString, splitter; limit::Integer=0, keepempty::Bool=false) =
        Base.split(s, splitter; limit=limit, keep=keepempty)
end

# https://github.com/JuliaLang/julia/pull/27828
if VERSION < v"0.7.0-beta.73"
    Base.mapslices(f, A::AbstractArray; dims=error("required keyword argument `dims` missing")) =
        mapslices(f, A, dims)
end

# https://github.com/JuliaLang/julia/pull/28302
if VERSION < v"0.7.0-beta2.169"
    const floatmin = realmin
    const floatmax = realmax
    export floatmin, floatmax
end

# https://github.com/JuliaLang/julia/pull/28303
if VERSION < v"0.7.0-beta2.143"
    export dropdims
    # https://github.com/JuliaLang/julia/pull/26660
    if VERSION >= v"0.7.0-DEV.4738"
        dropdims(
            X;
            dims = throw(
                UndefKeywordError("dropdims: keyword argument dims not assigned"))
        ) = squeeze(X, dims = dims)
    else
        dropdims(
            X;
            dims = throw(
                UndefKeywordError("dropdims: keyword argument dims not assigned"))
        ) = squeeze(X, dims)
    end
end

function rangedepwarn(;step=nothing, length=nothing, kwargs...)
    if step===nothing && length===nothing
        Base.depwarn("`range(start, stop)` (with neither `length` nor `step` given) is deprecated, use `range(start, stop=stop)` instead.", :range)
    end
end

if VERSION < v"1.1.0-DEV.506"
    function Base.range(start, stop; kwargs...)
        rangedepwarn(;kwargs...)
        range(start; stop=stop, kwargs...)
    end
end

# https://github.com/JuliaLang/julia/pull/30496
if VERSION < v"1.2.0-DEV.272"
    Base.@pure hasfield(::Type{T}, name::Symbol) where T =
        Base.fieldindex(T, name, false) > 0
    export hasfield
    if VERSION >= v"0.7-DEV.5136"
        hasproperty(x, s::Symbol) = s in propertynames(x)
        export hasproperty
    end
end

# https://github.com/JuliaLang/julia/pull/29259
if v"0.7.0" <= VERSION < v"1.1.0-DEV.594"
    Base.merge(a::NamedTuple, b::NamedTuple, cs::NamedTuple...) = merge(merge(a, b), cs...)
    Base.merge(a::NamedTuple) = a
end

# https://github.com/JuliaLang/julia/pull/33129
if v"0.7.0" <= VERSION < v"1.4.0-DEV.142"
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
if v"0.7.0" <= VERSION < v"1.3.0-alpha.8"
    Base.mod(i::Integer, r::Base.OneTo) = mod1(i, last(r))
    Base.mod(i::Integer, r::AbstractUnitRange{<:Integer}) = mod(i-first(r), length(r)) + first(r)
end

include("deprecated.jl")

end # module Compat
