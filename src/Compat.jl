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


include("compatmacro.jl")

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

# julia #26365
@static if isdefined(LinearAlgebra, :tr)
    const tr = LinearAlgebra.tr
else
    # 0.6
    const tr = LinearAlgebra.trace
end

if VERSION < v"0.7.0-DEV.1930"
    # no textwidth definition in Base
    export textwidth
    textwidth(c::Char) = charwidth(c)
    textwidth(c::AbstractString) = strwidth(c)
elseif v"0.7.0-DEV.2915" ≤ VERSION < v"0.7.0-DEV.3393"
    # textwidth definition moved to Unicode module
    import Unicode
    const textwidth = Unicode.textwidth
    export textwidth
end

# 0.7.0-DEV.2915
module Unicode
    export graphemes, textwidth, isvalid,
           islower, isupper, isalpha, isdigit, isxdigit, isnumeric, isalnum,
           iscntrl, ispunct, isspace, isprint, isgraph,
           lowercase, uppercase, titlecase, lcfirst, ucfirst

    if VERSION < v"0.7.0-DEV.2915"
        if VERSION < v"0.7.0-DEV.1930"
            import ..Compat: textwidth
        end

        isnumeric(c::Char) = isnumber(c)
        isassigned(c) = is_assigned_char(c)
        normalize(s::AbstractString; kws...) = normalize_string(s; kws...)
        normalize(s::AbstractString, nf::Symbol) = normalize_string(s, nf)
    else
        using Unicode
        import Unicode: isassigned, normalize # not exported from Unicode module due to conflicts
    end
end

# 0.7.0-DEV.3393
@static if VERSION < v"0.7.0-DEV.3393"
    import .Unicode.isnumeric
    export isnumeric
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



@static if !isdefined(Base, :Some)
    import Base: promote_rule, convert
    struct Some{T}
        value::T
    end
    promote_rule(::Type{Some{T}}, ::Type{Some{S}}) where {T,S<:T} = Some{T}
    promote_rule(::Type{Some{T}}, ::Type{Nothing}) where {T} = Union{Some{T}, Nothing}
    convert(::Type{Some{T}}, x::Some) where {T} = Some{T}(convert(T, x.value))
    convert(::Type{Union{Some{T}, Nothing}}, x::Some) where {T} = convert(Some{T}, x)

    convert(::Type{Union{T, Nothing}}, x::Any) where {T} = convert(T, x)
    convert(::Type{Nothing}, x::Any) = throw(MethodError(convert, (Nothing, x)))
    convert(::Type{Nothing}, x::Nothing) = nothing

    # Note: this is the definition of coalasce prior to 0.7.0-DEV.5278; kept to avoid
    # breakage in packages already using it
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
end
@static if !isdefined(Base, Symbol("@warn"))
    macro warn(msg, args...)
        return :(warn($(esc(msg)), prefix = "Warning: "))
    end
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
        argmin(x::Tuple) = indmin(x)
        argmax(x::AbstractArray) = CartesianIndex(ind2sub(x, indmax(x)))
        argmax(x::AbstractVector) = indmax(x)
        argmax(x::Associative) = first(Iterators.drop(keys(x), indmax(values(x))-1))
        argmax(x::Tuple) = indmax(x)
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

@static if VERSION < v"0.7.0-DEV.3272"
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
end

if VERSION >= v"0.7.0-DEV.3666"
    import UUIDs
else
    @eval module UUIDs
        if VERSION < v"0.7.0-DEV.3406"
            import Base.Random: uuid1, uuid4, uuid_version, UUID
        else
            import Random: uuid1, uuid4, uuid_version, UUID
        end
        export uuid1, uuid4, uuid_version, UUID
    end
end

# https://github.com/JuliaLang/julia/pull/26670
@static if VERSION < v"0.7.0-DEV.4062"
    trunc(x; digits = 0, base = 10) = Base.trunc(x, digits, base)
    floor(x; digits = 0, base = 10) = Base.floor(x, digits, base)
    ceil(x; digits = 0, base = 10) = Base.ceil(x, digits, base)
    function round(x; digits = nothing, sigdigits = nothing, base = 10)
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
    trunc(x; digits = 0, base = 10) = Base.trunc(x, digits, base = base)
    floor(x; digits = 0, base = 10) = Base.floor(x, digits, base = base)
    ceil(x; digits = 0, base = 10) = Base.ceil(x, digits, base = base)
    function round(x; digits = nothing, sigdigits = nothing, base = 10)
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
elseif VERSION < v"0.7.0-beta2.86"
    # https://github.com/JuliaLang/julia/pull/28199
    trunc(x; digits = 0, base = 10) = Base.trunc(x, digits = digits, base = base)
    floor(x; digits = 0, base = 10) = Base.floor(x, digits = digits, base = base)
    ceil(x; digits = 0, base = 10) = Base.ceil(x, digits = digits, base = base)
    function round(x; digits = nothing, sigdigits = nothing, base = 10)
        if digits === nothing && sigdigits === nothing
            Base.round(x, digits = 0, base = base)
        else
            Base.round(x, digits = digits, sigdigits = sigdigits, base = base)
        end
    end
else
    trunc(x; digits = 0, base = 10) = Base.trunc(x, digits = digits, base = base)
    floor(x; digits = 0, base = 10) = Base.floor(x, digits = digits, base = base)
    ceil(x; digits = 0, base = 10) = Base.ceil(x, digits = digits, base = base)
    round(x; digits = nothing, sigdigits = nothing, base = 10) = Base.round(x, digits = digits, sigdigits = sigdigits, base = base)
end

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
            buf = Base.IOBuffer(Base.StringVector(size), flags[1], flags[2], Int(maxsize))
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
elseif VERSION < v"1.0.0-DEV.57"
    import Base: LinRange
    range(start; kwargs...) = Base.range(start; kwargs...)
else
    import Base: range # import as it is further extended below
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
end

if VERSION < v"0.7.0-DEV.4585"
    export isuppercase, islowercase, uppercasefirst, lowercasefirst
    const isuppercase = isupper
    const islowercase = islower
    const uppercasefirst = ucfirst
    const lowercasefirst = lcfirst
end

if VERSION < v"0.7.0-DEV.4064"
    for f in (:mean, :median, :var, :varm, :std, :cov, :cor)
        @eval import .Statistics: $f # compatibility with old Compat versions
    end
    for f in (:cumsum, :cumprod, :sum, :prod, :maximum, :minimum, :all, :any)
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
    @eval sort(a::AbstractArray; dims=nothing, kwargs...) =
        dims===nothing ? Base.sort(a; kwargs...) : Base.sort(a, dims; kwargs...)
    for f in (:cumsum!, :cumprod!)
        @eval $f(out, a; dims=nothing) =
            dims===nothing ? Base.$f(out, a) : Base.$f(out, a, dims)
    end
end
if VERSION < v"0.7.0-DEV.4064"
    mapreduce(f, op, a::AbstractArray; dims=nothing, init=nothing) =
        init === nothing ? (dims===nothing ? Base.mapreduce(f, op, a) : Base.mapreducedim(f, op, a, dims)) :
                           (dims===nothing ? Base.mapreduce(f, op, init, a) : Base.mapreducedim(f, op, a, dims, init))
    reduce(op, a::AbstractArray; dims=nothing, init=nothing) =
        init === nothing ? (dims===nothing ? Base.reduce(op, a) : Base.reducedim(op, a, dims)) :
                           (dims===nothing ? Base.reduce(op, init, a) : Base.reducedim(op, a, dims, init))
    accumulate!(op, out, a; dims=nothing) =
        dims===nothing ? Base.accumulate!(op, out, a) : Base.accumulate!(op, out, a, dims)
    # kept for compatibility with early adopters
    mapreduce(f, op, v0, a::AbstractArray; dims=nothing) =
        mapreduce(f, op, a, dims=dims, init=v0)
    reduce(op, v0, a::AbstractArray; dims=nothing) =
        reduce(op, a, dims=dims, init=v0)
elseif VERSION < v"0.7.0-beta.81" # julia#27711
    mapreduce(f, op, a::AbstractArray; dims=nothing, init=nothing) =
        init === nothing ? (dims===nothing ? Base.mapreduce(f, op, a) : Base.mapreduce(f, op, a, dims=dims)) :
                           (dims===nothing ? Base.mapreduce(f, op, init, a) : Base.mapreduce(f, op, init, a, dims=dims))
    reduce(op, a::AbstractArray; dims=nothing, init=nothing) =
        init === nothing ? (dims===nothing ? Base.reduce(op, a) : Base.reduce(op, a, dims=dims)) :
                           (dims===nothing ? Base.reduce(op, init, a) : Base.reduce(op, init, a, dims=dims))
end
if VERSION < v"0.7.0-beta.81" # julia#27711
    mapreduce(f, op, itr; init=nothing) =
        init === nothing ? Base.mapreduce(f, op, itr) : Base.mapreduce(f, op, init, itr)
    reduce(op, itr; init=nothing) =
        init === nothing ? Base.reduce(op, itr) : Base.reduce(op, init, itr)
end
if VERSION < v"0.7.0-DEV.4534"
    reverse(a::AbstractArray; dims=nothing) =
        dims===nothing ? Base.reverse(a) : Base.flipdim(a, dims)
end
if VERSION < v"0.7.0-DEV.4738"
    Base.squeeze(A; dims=error("squeeze: keyword argument dims not assigned")) = squeeze(A, dims)
end
if VERSION < v"0.7.0-DEV.5165" # julia#27163
    cat(X...; dims = throw(UndefKeywordError("cat: keyword argument dims not assigned"))) = Base.cat(dims, X...)
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

if VERSION < v"0.7.0-DEV.3977" #26039
    Base.repeat(A::AbstractArray, counts::Integer...) = Base.repeat(A, outer = counts)
    Base.repeat(a::AbstractVecOrMat, m::Integer, n::Integer=1) = Base.repmat(a, m, n)
    Base.repeat(a::AbstractVector, m::Integer) = Base.repmat(a, m)
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
        :(Base.cfunction($(esc(f)), $(esc(rt)), Tuple{$(esc(tup))...}))
    end
    export @cfunction
end

if VERSION < v"0.7.0-DEV.2920" # julia#24999
    Base.length(s::AbstractString, i::Integer, j::Integer) = length(s, Int(i), Int(j))
    function Base.length(s::AbstractString, i::Int, j::Int)
        @boundscheck begin
            0 < i ≤ ncodeunits(s)+1 || throw(BoundsError(s, i))
            0 ≤ j < ncodeunits(s)+1 || throw(BoundsError(s, j))
        end
        n = 0
        for k = i:j
            @inbounds n += isvalid(s, k)
        end
        return n
    end
    Base.codeunit(s::String) = UInt8
    Base.codeunit(s::SubString) = codeunit(s.string)
end
if !isdefined(Base, :thisind) # #24414
    thisind(s::AbstractString, i::Integer) = thisind(s, Int(i))
    function thisind(s::AbstractString, i::Int)
        z = ncodeunits(s) + 1
        i == z && return i
        @boundscheck 0 ≤ i ≤ z || throw(BoundsError(s, i))
        @inbounds while 1 < i && !isvalid(s, i)
            i -= 1
        end
        return i
    end
    export thisind
end
if VERSION < v"0.7.0-DEV.2019" # julia#23805
    Base.prevind(s::AbstractString, i::Integer, n::Integer) = prevind(s, Int(i), Int(n))
    Base.nextind(s::AbstractString, i::Integer, n::Integer) = nextind(s, Int(i), Int(n))
    function Base.nextind(s::AbstractString, i::Int, n::Int)
        n < 0 && throw(ArgumentError("n cannot be negative: $n"))
        z = ncodeunits(s)
        @boundscheck 0 ≤ i ≤ z || throw(BoundsError(s, i))
        n == 0 && return thisind(s, i) == i ? i : throw(BoundsError(s, i))
        while n > 0 && i < z
            @inbounds n -= isvalid(s, i += 1)
        end
        return i + n
    end
    function Base.prevind(s::AbstractString, i::Int, n::Int)
        n < 0 && throw(ArgumentError("n cannot be negative: $n"))
        z = ncodeunits(s) + 1
        @boundscheck 0 < i ≤ z || throw(BoundsError(s, i))
        n == 0 && return thisind(s, i) == i ? i : throw(BoundsError(s, i))
        while n > 0 && 1 < i
            @inbounds n -= isvalid(s, i -= 1)
        end
        return i - n
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
    function range(start, stop; kwargs...)
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
