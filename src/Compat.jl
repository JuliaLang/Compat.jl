module Compat

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

include("deprecated.jl")

end # module Compat
