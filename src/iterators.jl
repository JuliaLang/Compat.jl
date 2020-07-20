module CompatIterators

using Base:
    @inline, Pair, AbstractDict, IndexLinear, IndexCartesian, IndexStyle, AbstractVector, Vector,
    tail, tuple_type_head, tuple_type_tail, tuple_type_cons, SizeUnknown, HasLength, HasShape,
    IsInfinite, EltypeUnknown, HasEltype, OneTo, @propagate_inbounds, Generator, AbstractRange,
    LinearIndices, (:), |, +, -, !==, !, <=, <, missing, any, @boundscheck, @inbounds

import .Base:
    first, last,
    isempty, length, size, axes, ndims,
    eltype, IteratorSize, IteratorEltype,
    haskey, keys, values, pairs,
    getindex, setindex!, get, iterate,
    popfirst!, isdone, peek

const Iterators = @__MODULE__
export Iterators

# Import exported APIs
for n in names(Base.Iterators)
    n === :Iterators && continue
    @eval begin
        using Base.Iterators: $n
        export $n
    end
end

# Import unexported public APIs
using Base.Iterators: filter

# https://github.com/JuliaLang/julia/pull/33437
if VERSION < v"1.4.0-DEV.291"  # 5f013d82f92026f7dfbe4234f283658beb1f8a2a
    export takewhile, dropwhile

    # takewhile
    struct TakeWhile{I,P<:Function}
        pred::P
        xs::I
    end

    takewhile(pred,xs) = TakeWhile(pred,xs)

    function iterate(ibl::TakeWhile, itr...)
        y = iterate(ibl.xs,itr...)
        y === nothing && return nothing
        ibl.pred(y[1]) || return nothing
        y
    end

    IteratorSize(::Type{<:TakeWhile}) = SizeUnknown()
    eltype(::Type{TakeWhile{I,P}}) where {I,P} = eltype(I)
    IteratorEltype(::Type{TakeWhile{I,P}}) where {I,P} = IteratorEltype(I)

    # dropwhile
    struct DropWhile{I,P<:Function}
        pred::P
        xs::I
    end

    dropwhile(pred,itr) = DropWhile(pred,itr)

    iterate(ibl::DropWhile,itr) = iterate(ibl.xs, itr)
    function iterate(ibl::DropWhile)
        y = iterate(ibl.xs)
        while y !== nothing
            ibl.pred(y[1]) || break
            y = iterate(ibl.xs,y[2])
        end
        y
    end

    IteratorSize(::Type{<:DropWhile}) = SizeUnknown()
    eltype(::Type{DropWhile{I,P}}) where {I,P} = eltype(I)
    IteratorEltype(::Type{DropWhile{I,P}}) where {I,P} = IteratorEltype(I)
end

# https://github.com/JuliaLang/julia/pull/34352
if VERSION < v"1.6.0-DEV.258"  # 1f8b44204fafb1caabc6a1cd6ca39458a550e2fc
    map(f, args...) = Base.Generator(f, args...)
else
    using Base.Iterators: map
end

end  # module

const Iterators = CompatIterators
