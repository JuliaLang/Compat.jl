if !isdefined(Base, :get_extension)
    # LinearAlgebra is a weakdep, but Julia is old enough to ignore that, so we
    # can import and use it here
    import LinearAlgebra
    Base.@deprecate_binding set_num_threads LinearAlgebra.BLAS.set_num_threads false
    Base.@deprecate_binding get_num_threads LinearAlgebra.BLAS.get_num_threads false
else
    # deprecation is done in the CompatLinearAlgebraExt package extension, but
    # the functions need to be declared here
    function set_num_threads end
    function get_num_threads end
end

Base.@deprecate_binding parseatom Meta.parseatom false
Base.@deprecate_binding parseall Meta.parseall false
import UUIDs
Base.@deprecate_binding uuid5 UUIDs.uuid5 true

# https://github.com/JuliaLang/julia/pull/46104
# reverted in https://github.com/JuliaLang/julia/pull/52010
if VERSION < v"1.10.0-DEV.1404" ||
    (VERSION >= v"1.10-rc2" && VERSION < v"1.11.0-") ||
    VERSION >= v"1.11.0-DEV.924"
    using Base: Ordering, Forward, ord, lt, tail, copymutable, DEFAULT_STABLE, IteratorSize, HasShape, IsInfinite
    function Base.sort(v; kws...)
        Base.depwarn("sorting arbitrary iterables is deprecated", :sort)
        if v isa AbstractString
            throw(ArgumentError("sort(::AbstractString) is not supported"))
        end
        if v isa NTuple
            return _sort(v; kws...)
        end
        if v isa Tuple
            throw(ArgumentError("sort(::Tuple) is only supported for NTuples"))
        end
        size = IteratorSize(v)
        size == HasShape{0}() && throw(ArgumentError("$v cannot be sorted"))
        size == IsInfinite() && throw(ArgumentError("infinite iterator $v cannot be sorted"))
        sort!(copymutable(v); kws...)
    end

    function _sort(x::NTuple{N}; lt::Function=isless, by::Function=identity,
                  rev::Union{Bool,Nothing}=nothing, order::Ordering=Forward) where N
        o = ord(lt,by,rev,order)
        if N > 9
            v = sort!(copymutable(x), DEFAULT_STABLE, o)
            tuple((v[i] for i in 1:N)...)
        else
            _sort(x, o)
        end
    end
    _sort(x::Union{NTuple{0}, NTuple{1}}, o::Ordering) = x
    function _sort(x::NTuple, o::Ordering)
        a, b = Base.IteratorsMD.split(x, Val(length(x)>>1))
        merge(_sort(a, o), _sort(b, o), o)
    end
    merge(x::NTuple, y::NTuple{0}, o::Ordering) = x
    merge(x::NTuple{0}, y::NTuple, o::Ordering) = y
    merge(x::NTuple{0}, y::NTuple{0}, o::Ordering) = x # Method ambiguity
    merge(x::NTuple, y::NTuple, o::Ordering) =
        (lt(o, y[1], x[1]) ? (y[1], merge(x, tail(y), o)...) : (x[1], merge(tail(x), y, o)...))
end
