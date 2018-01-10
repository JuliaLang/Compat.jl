__precompile__()

module Compat

using Base.Meta

@static if !isdefined(Base, Symbol("@nospecialize"))
    # 0.7
    macro nospecialize(arg)
        if isa(arg, Symbol)
            # @nospecialize(arg)
            return :($(esc(arg))::ANY)
        elseif isa(arg, Expr) && arg.head == :(::)
            # @nospecialize(arg::typ)
            # unsupported: needs ::ANY which would change dispatch as determined by ::typ
        elseif isa(arg, Expr) && arg.head == :(=)
            # @nospecialize(arg=val)
            arg, val = arg.args
            if isa(arg, Expr) && arg.head == :(::)
                # @nospecialize(arg::typ=val)
                # unsupported (see above), but generate a kw arg
                arg, typ = arg.args
                return Expr(:kw, :($(esc(arg))::$(esc(typ))), esc(val))
            else
                return Expr(:kw, :($(esc(arg))::ANY), esc(val))
            end
        end
        return esc(arg)
    end
    export @nospecialize
end

function _compat(ex::Expr)
    if VERSION < v"0.7.0-DEV.880"
        if ex.head == :curly && ex.args[1] == :CartesianRange && length(ex.args) >= 2
            a = ex.args[2]
            if a != :CartesianIndex && !(isa(a, Expr) && a.head == :curly && a.args[1] == :CartesianIndex)
                return Expr(:curly, :CartesianRange, Expr(:curly, :CartesianIndex, ex.args[2]))
            end
        end
    end
    if VERSION < v"0.7.0-DEV.2562"
        if ex.head == :call && ex.args[1] == :finalizer
            ex.args[2], ex.args[3] = ex.args[3], ex.args[2]
        end
    end
    return Expr(ex.head, map(_compat, ex.args)...)
end

_compat(ex) = ex

macro compat(ex...)
    if length(ex) != 1
        throw(ArgumentError("@compat called with wrong number of arguments: $ex"))
    end
    esc(_compat(ex[1]))
end


export @compat

# https://github.com/JuliaLang/julia/pull/22064
@static if !isdefined(Base, Symbol("@__MODULE__"))
    # 0.7
    export @__MODULE__
    macro __MODULE__()
        return current_module()
    end
    Base.expand(mod::Module, x::ANY) = eval(mod, :(expand($(QuoteNode(x)))))
    Base.macroexpand(mod::Module, x::ANY) = eval(mod, :(macroexpand($(QuoteNode(x)))))
    Base.include_string(mod::Module, code::String, fname::String) =
        eval(mod, :(include_string($code, $fname)))
    Base.include_string(mod::Module, code::AbstractString, fname::AbstractString="string") =
        eval(mod, :(include_string($code, $fname)))
end

import Base.ExponentialBackOff
import Base.retry

# PR #17302
# Provide a non-deprecated version of `@vectorize_(1|2)arg` macro which defines
# deprecated version of the function so that the depwarns can be fixed without
# breaking users.
# Packages are expected to use this to maintain the old API until all users
# of the deprecated vectorized function have migrated.
# These macros should raise a depwarn when the `0.5` support is dropped from
# `Compat` and be dropped when the support for `0.6` is dropped from `Compat`.
# Modified based on the version copied from 0.6 Base.
if VERSION < v"0.7.0-DEV.1211"
    macro dep_vectorize_1arg(S, f)
        S = esc(S)
        f = esc(f)
        T = esc(:T)
        x = esc(:x)
        AbsArr = esc(:AbstractArray)
        ## Depwarn to be enabled when 0.5 support is dropped.
        depwarn("Implicit vectorized function is deprecated in favor of compact broadcast syntax.",
                Symbol("@dep_vectorize_1arg"))
        :(@deprecate $f{$T<:$S}($x::$AbsArr{$T}) @compat($f.($x)))
    end

    macro dep_vectorize_2arg(S, f)
        S = esc(S)
        f = esc(f)
        T1 = esc(:T1)
        T2 = esc(:T2)
        x = esc(:x)
        y = esc(:y)
        AbsArr = esc(:AbstractArray)
        ## Depwarn to be enabled when 0.5 support is dropped.
        depwarn("Implicit vectorized function is deprecated in favor of compact broadcast syntax.",
                Symbol("@dep_vectorize_2arg"))
        quote
            @deprecate $f{$T1<:$S}($x::$S, $y::$AbsArr{$T1}) @compat($f.($x,$y))
            @deprecate $f{$T1<:$S}($x::$AbsArr{$T1}, $y::$S) @compat($f.($x,$y))
            @deprecate $f{$T1<:$S,$T2<:$S}($x::$AbsArr{$T1}, $y::$AbsArr{$T2}) @compat($f.($x,$y))
        end
    end
else
    macro dep_vectorize_1arg(S, f)
        AbstractArray = GlobalRef(Base, :AbstractArray)
        return esc(:(@deprecate $f(x::$AbstractArray{T}) where {T<:$S} $f.(x)))
    end

    macro dep_vectorize_2arg(S, f)
        AbstractArray = GlobalRef(Base, :AbstractArray)
        return esc(quote
            @deprecate $f(x::$S, y::$AbstractArray{T1}) where {T1<:$S} $f.(x, y)
            @deprecate $f(x::$AbstractArray{T1}, y::$S) where {T1<:$S} $f.(x, y)
            @deprecate $f(x::$AbstractArray{T1}, y::$AbstractArray{T2}) where {T1<:$S, T2<:$S} $f.(x, y)
        end)
    end
end

# https://github.com/JuliaLang/julia/pull/19784
@static if isdefined(Base, :invokelatest)
    # https://github.com/JuliaLang/julia/pull/22646
    if VERSION < v"0.7.0-DEV.1139"
        function invokelatest(f, args...; kwargs...)
            inner() = f(args...; kwargs...)
            Base.invokelatest(inner)
        end
    else
        import Base.invokelatest
    end
end

# https://github.com/JuliaLang/julia/pull/21197
if VERSION < v"0.7.0-DEV.257"
    # allow the elements of the Cmd to be accessed as an array or iterator
    for f in (:length, :endof, :start, :eachindex, :eltype, :first, :last)
        @eval Base.$f(cmd::Cmd) = $f(cmd.exec)
    end
    for f in (:next, :done, :getindex)
        @eval Base.$f(cmd::Cmd, i) = $f(cmd.exec, i)
    end
end

# https://github.com/JuliaLang/julia/pull/22475
@static if VERSION < v"0.7.0-DEV.843"
    import Base: Val
    (::Type{Val})(x) = (Base.@_pure_meta; Val{x}())
    # Also add methods for Val(x) that were previously Val{x}
    import Base: reshape
    reshape(parent::AbstractArray, ndims::Val{N}) where {N} = reshape(parent, Val{N})
    import Base: ntuple
    ntuple(f::F, ::Val{N}) where {F,N} = ntuple(f, Val{N})
end

# https://github.com/JuliaLang/julia/pull/22629
if VERSION < v"0.7.0-DEV.848"
    import Base: logdet
    logdet(A) = log(det(A))
end

# https://github.com/JuliaLang/julia/pull/22633
if VERSION < v"0.7.0-DEV.1041"
    import Base.LinAlg: chol, chol!
    chol!(J::UniformScaling, uplo) = UniformScaling(chol!(J.λ, uplo))
    chol(J::UniformScaling, args...) = UniformScaling(chol(J.λ, args...))
end

# https://github.com/JuliaLang/julia/pull/21746
const macros_have_sourceloc = VERSION >= v"0.7-" && length(:(@test).args) == 2

# https://github.com/JuliaLang/julia/pull/22182
module Sys
    const KERNEL = Base.Sys.KERNEL
    @static if VERSION < v"0.7.0-DEV.914"
        isapple(k::Symbol=KERNEL)   = k in (:Darwin, :Apple)
        isbsd(k::Symbol=KERNEL)     = isapple(k) || k in (:FreeBSD, :OpenBSD, :NetBSD, :DragonFly)
        islinux(k::Symbol=KERNEL)   = k == :Linux
        isunix(k::Symbol=KERNEL)    = isbsd(k) || islinux(k)
        iswindows(k::Symbol=KERNEL) = k in (:Windows, :NT)
    else
        import Base.Sys: isapple, isbsd, islinux, isunix, iswindows
    end

    # https://github.com/JuliaLang/julia/pull/25102
    # NOTE: This needs to be in an __init__ because JULIA_HOME is not
    # defined when building system images.
    function __init__()
        global BINDIR = VERSION < v"0.7.0-DEV.3073" ? JULIA_HOME : Base.Sys.BINDIR
    end
end

@static if VERSION < v"0.7.0-DEV.892"
    fieldcount(t) = nfields(t)
    export fieldcount
end

if VERSION < v"0.7.0-DEV.1053"
    Base.read(obj::IO, ::Type{String}) = readstring(obj)
    Base.read(obj::AbstractString, ::Type{String}) = readstring(obj)
    Base.read(obj::Cmd, ::Type{String}) = readstring(obj)
end

# https://github.com/JuliaLang/julia/pull/20005
if VERSION < v"0.7.0-DEV.896"
    Base.InexactError(name::Symbol, T, val) = InexactError()
end

# https://github.com/JuliaLang/julia/pull/22751
if VERSION < v"0.7.0-DEV.924"
    Base.DomainError(val) = DomainError()
    Base.DomainError(val, msg) = DomainError()
end

# https://github.com/JuliaLang/julia/pull/22761
if VERSION < v"0.7.0-DEV.1285"
    Base.OverflowError(msg) = OverflowError()
end

if VERSION < v"0.7.0-DEV.755"
    # This is a hack to only add keyword signature that won't work on all julia versions.
    # However, since we really only need to support a few (0.5, 0.6 and early 0.7) versions
    # this should be good enough.
    let Tf = typeof(cov), Tkw = Core.Core.kwftype(Tf)
        @eval begin
            @inline function _get_corrected(kws)
                corrected = true
                nkw = length(kws) >> 1
                for i in 1:nkw
                    if kws[i * 2 - 1] !== :corrected
                        Base.kwerr(kws)
                    end
                    corrected = kws[i * 2]
                end
                return corrected::Bool
            end
            (::$Tkw)(kws::Vector{Any}, ::$Tf, x::AbstractVector) = cov(x, _get_corrected(kws))
            (::$Tkw)(kws::Vector{Any}, ::$Tf, X::AbstractVector, Y::AbstractVector) =
                cov(X, Y, _get_corrected(kws))
            (::$Tkw)(kws::Vector{Any}, ::$Tf, x::AbstractMatrix, vardim::Int) =
                cov(x, vardim, _get_corrected(kws))
            (::$Tkw)(kws::Vector{Any}, ::$Tf, X::AbstractVecOrMat, Y::AbstractVecOrMat,
                     vardim::Int) = cov(X, Y, vardim, _get_corrected(kws))
        end
    end
end

# 0.7.0-DEV.1415
@static if !isdefined(Base, :adjoint)
    const adjoint = ctranspose
    const adjoint! = ctranspose!
    export adjoint, adjoint!
end

# 0.7.0-DEV.1592
@static if !isdefined(Base, :MathConstants)
    @eval module MathConstants
    # All other ones are already exported by Base (so should be already in the users namespace)
    # and will be automatically be in this module.
    export ℯ
    const ℯ = e
    end
    const ℯ = e
    export ℯ
else
    import Base.MathConstants
end

# 0.7.0-DEV.1535
@static if !isdefined(Base, :partialsort)
    const partialsort = select
    const partialsort! = select!
    const partialsortperm = selectperm
    const partialsortperm! = selectperm!
    export partialsort, partialsort!, partialsortperm, partialsortperm!
end

# 0.7.0-DEV.1660
@static if !isdefined(Base, :pairs)
    pairs(collection) = Base.Generator(=>, keys(collection), values(collection))
    pairs(a::Associative) = a

    pairs(::IndexLinear,    A::AbstractArray) = Iterators.IndexValue(A, linearindices(A))
    pairs(::IndexCartesian, A::AbstractArray) = Iterators.IndexValue(A, CartesianRange(indices(A)))

    Base.keys(a::AbstractArray) = CartesianRange(indices(a))
    Base.keys(a::AbstractVector) = linearindices(a)
    Base.keys(s::IndexStyle, A::AbstractArray, B::AbstractArray...) = eachindex(s, A, B...)

    Base.values(itr) = itr
end

# 0.7.0-DEV.1721
@static if !isdefined(Base, :AbstractRange)
    const AbstractRange = Range
    export AbstractRange
end

if VERSION < v"0.7.0-DEV.1325"
    function Base.rtoldefault(x, y, atol::Real)
        T = isa(x, Type) ? x : typeof(x)
        S = isa(y, Type) ? y : typeof(y)
        rtol = max(Base.rtoldefault(real(T)), Base.rtoldefault(real(S)))
        return atol > 0 ? zero(rtol) : rtol
    end
end

# 0.7.0-DEV.1775
@static if !isdefined(Base, :isconcrete)
    const isconcrete = isleaftype
    export isconcrete
end

# 0.7.0-DEV.2005
if VERSION < v"0.7.0-DEV.2005"
    const Mmap = Base.Mmap
    const Test = Base.Test
    @eval module SharedArrays
        if isdefined(Base, :Distributed)
            using Base.Distributed.procs
        else
            using Base.procs
        end
        export SharedArray, SharedMatrix, SharedVector, indexpids, localindexes, sdata,
               procs
    end
    const DelimitedFiles = Base.DataFmt
else
    import Test, SharedArrays, Mmap, DelimitedFiles
end

if VERSION < v"0.7.0-DEV.2575"
    const Dates = Base.Dates
else
    import Dates
end

if VERSION < v"0.7.0-DEV.3052"
    const Printf = Base.Printf
else
    import Printf
end

if VERSION < v"0.7.0-DEV.2655"
    @eval module IterativeEigensolvers
        using Base: eigs, svds
        export eigs, svds
    end
elseif VERSION < v"0.7.0-DEV.3019"
    @eval module IterativeEigensolvers
        using IterativeEigenSolvers: eigs, svds
        export eigs, svds
    end
else
    import IterativeEigensolvers
end

if VERSION < v"0.7.0-DEV.2609"
    @eval module SuiteSparse
        if Base.USE_GPL_LIBS
            using Base.SparseArrays: CHOLMOD, SPQR, UMFPACK
        end
        using Base.SparseArrays: increment, increment!, decrement, decrement!
    end
else
    import SuiteSparse
end

# 0.7.0-DEV.1993
@static if !isdefined(Base, :EqualTo)
    struct EqualTo{T} <: Function
        x::T
        EqualTo(x::T) where {T} = new{T}(x)
    end
    (f::EqualTo)(y) = isequal(f.x, y)
    const equalto = EqualTo
    export equalto
end

# 0.7.0-DEV.912
if VERSION < v"0.7.0-DEV.912"
    import Base.*
    (*)(s1::Union{Char,AbstractString}, ss::Union{Char,AbstractString}...) = string(s1, ss...)
end

# 0.7.0-DEV.2318
@static if !isdefined(Base, :BitSet)
    const BitSet = IntSet
    export BitSet
end

# 0.7.0-DEV.2116
@static if VERSION < v"0.7.0-DEV.2116"
    import Base: spdiagm
    function spdiagm(kv::Pair...)
        I, J, V = Base.SparseArrays.spdiagm_internal(last.(kv), first.(kv))
        m = max(Base.SparseArrays.dimlub(I), Base.SparseArrays.dimlub(J))
        return sparse(I, J, V, m, m)
    end
end

# 0.7.0-DEV.2161
@static if VERSION < v"0.7.0-DEV.2161"
    import Base: diagm
    function diagm(kv::Pair...)
        T = promote_type(map(x -> eltype(x.second), kv)...)
        n = mapreduce(x -> length(x.second) + abs(x.first), max, kv)
        A = zeros(T, n, n)
        for p in kv
            inds = diagind(A, p.first)
            for (i, val) in enumerate(p.second)
                A[inds[i]] += val
            end
        end
        return A
    end
end

# 0.7.0-DEV.2338
@static if VERSION >= v"0.7.0-DEV.2338"
    import Base64
else
    import Base.Base64
end

@static if VERSION < v"0.7.0-DEV.2377"
    Matrix{T}(s::UniformScaling, dims::Dims{2}) where {T} = setindex!(zeros(T, dims), T(s.λ), diagind(dims...))
    Matrix{T}(s::UniformScaling, m::Integer, n::Integer) where {T} = Matrix{T}(s, Dims((m, n)))

    SparseMatrixCSC{Tv,Ti}(s::UniformScaling, m::Integer, n::Integer) where {Tv,Ti} = SparseMatrixCSC{Tv,Ti}(s, Dims((m, n)))
    SparseMatrixCSC{Tv}(s::UniformScaling, m::Integer, n::Integer) where {Tv} = SparseMatrixCSC{Tv}(s, Dims((m, n)))
    SparseMatrixCSC{Tv}(s::UniformScaling, dims::Dims{2}) where {Tv} = SparseMatrixCSC{Tv,Int}(s, dims)
    function SparseMatrixCSC{Tv,Ti}(s::UniformScaling, dims::Dims{2}) where {Tv,Ti}
        @boundscheck first(dims) < 0 && throw(ArgumentError("first dimension invalid ($(first(dims)) < 0)"))
        @boundscheck last(dims) < 0 && throw(ArgumentError("second dimension invalid ($(last(dims)) < 0)"))
        iszero(s.λ) && return spzeros(Tv, Ti, dims...)
        m, n, k = dims..., min(dims...)
        nzval = fill!(Vector{Tv}(k), Tv(s.λ))
        rowval = copy!(Vector{Ti}(k), 1:k)
        colptr = copy!(Vector{Ti}(n + 1), 1:(k + 1))
        for i in (k + 2):(n + 1) colptr[i] = (k + 1) end
        SparseMatrixCSC{Tv,Ti}(dims..., colptr, rowval, nzval)
    end
end
@static if VERSION < v"0.7.0-DEV.2543"
    Array{T}(s::UniformScaling, dims::Dims{2}) where {T} = Matrix{T}(s, dims)
    Array{T}(s::UniformScaling, m::Integer, n::Integer) where {T} = Matrix{T}(s, m, n)
end

# https://github.com/JuliaLang/julia/pull/23271
@static if VERSION < v"0.7.0-DEV.1472"
    Base.IOContext(io::IO, arg1::Pair, arg2::Pair, args::Pair...) = IOContext(IOContext(io, arg1), arg2, args...)
    # needed for ambiguity resolution
    Base.IOContext(io::IOContext, arg1::Pair, arg2::Pair) = IOContext(IOContext(io, arg1), arg2)
end

# 0.7.0-DEV.2581
@static if !isdefined(Base, :Uninitialized)
    struct Uninitialized end
    Array{T}(::Uninitialized, args...) where {T} = Array{T}(args...)
    Array{T,N}(::Uninitialized, args...) where {T,N} = Array{T,N}(args...)
    Vector(::Uninitialized, args...) = Vector(args...)
    Matrix(::Uninitialized, args...) = Matrix(args...)

    BitArray{N}(::Uninitialized, args...) where {N} = BitArray{N}(args...)
    BitArray(::Uninitialized, args...) = BitArray(args...)

    const uninitialized = Uninitialized()
    export Uninitialized, uninitialized
end

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

# 0.7.0-DEV.2915
module Unicode
    export graphemes, textwidth, isvalid,
           islower, isupper, isalpha, isdigit, isxdigit, isnumeric, isalnum,
           iscntrl, ispunct, isspace, isprint, isgraph,
           lowercase, uppercase, titlecase, lcfirst, ucfirst

    if VERSION < v"0.7.0-DEV.2915"
        # 0.7.0-DEV.1930
        if !isdefined(Base, :textwidth)
            textwidth(c::Char) = charwidth(c)
            textwidth(c::AbstractString) = strwidth(c)
        end

        isnumeric(c::Char) = isnumber(c)

        # 0.6.0-dev.1404 (https://github.com/JuliaLang/julia/pull/19469)
        if !isdefined(Base, :titlecase)
            titlecase(c::Char) = isascii(c) ? ('a' <= c <= 'z' ? c - 0x20 : c) :
                Char(ccall(:utf8proc_totitle, UInt32, (UInt32,), c))

            function titlecase(s::AbstractString)
                startword = true
                b = IOBuffer()
                for c in s
                    if isspace(c)
                        print(b, c)
                        startword = true
                    else
                        print(b, startword ? titlecase(c) : c)
                        startword = false
                    end
                end
                return String(take!(b))
            end
        end
    else
        using Unicode
    end
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

@static if !isdefined(Base, :Some)
    import Base: promote_rule, convert
    struct Some{T}
        value::T
    end
    promote_rule(::Type{Some{S}}, ::Type{Some{T}}) where {S,T} = Some{promote_type(S, T)}
    promote_rule(::Type{Some{T}}, ::Type{Nothing}) where {T} = Union{Some{T}, Nothing}
    convert(::Type{Some{T}}, x::Some) where {T} = Some{T}(convert(T, x.value))
    convert(::Type{Union{Some{T}, Nothing}}, x::Some) where {T} = convert(Some{T}, x)
    convert(::Type{Union{T, Nothing}}, x::Any) where {T} = convert(T, x)

    convert(::Type{Nothing}, x::Any) = throw(MethodError(convert, (Nothing, x)))
    convert(::Type{Nothing}, x::Nothing) = nothing
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

# 0.7.0-DEV.3155
@static if !isdefined(Base, :pushfirst!)
    const pushfirst! = unshift!
    const popfirst! = shift!
    export pushfirst!, popfirst!
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

@static if VERSION < v"0.7.0-DEV.3025"
    import Base: convert, ndims, getindex, size, length, eltype,
                 start, next, done, first, last
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
        if any(map(>, iterfirst.I, iterlast.I))
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

    length(iter::CartesianIndices) = prod(size(iter))

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
    @inline function Base.getindex(iter::LinearIndices{N,R}, I::Vararg{Int, N}) where {N,R}
        dims = length.(iter.indices)
        #without the inbounds, this is slower than Base._sub2ind(iter.indices, I...)
        @inbounds result = reshape(1:prod(dims), dims)[(I .- first.(iter.indices) .+ 1)...]
        return result
    end
end


include("deprecated.jl")

end # module Compat
