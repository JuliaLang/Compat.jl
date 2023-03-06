module Compat

if VERSION < v"1.9.0-"
    # `Dates` is a weakdep, so won't be available on Julia versions with weakdep support,
    # i.e. Julia 1.9 and later, so this `using` has to be inside the conditional.
    # Should a post-1.9 feature of Dates be added to Compat, the way forward will be a
    # package extension
    using Dates: Period, CompoundPeriod
end

include("compatmacro.jl")

# NOTE these `@inline` and `@noinline` definitions overwrite the definitions implicitly
# imported from Base and so should happen before any usages of them within this module

# https://github.com/JuliaLang/julia/pull/41312: `@inline`/`@noinline` annotations within a function body
@static if !hasmethod(getfield(Base, Symbol("@inline")), (LineNumberNode,Module))
    macro inline()   Expr(:meta, :inline)   end
    macro noinline() Expr(:meta, :noinline) end
end

# https://github.com/JuliaLang/julia/pull/41328: callsite annotations of inlining
@static if !isdefined(Base, :annotate_meta_def_or_block)
    macro inline(ex)   annotate_meta_def_or_nothing(ex, :inline)   end
    macro noinline(ex) annotate_meta_def_or_nothing(ex, :noinline) end
    function annotate_meta_def_or_nothing(@nospecialize(ex), meta::Symbol)
        inner = unwrap_macrocalls(ex)
        if is_function_def(inner)
            # annotation on a definition
            return esc(Base.pushmeta!(ex, meta))
        else
            # do nothing
            return esc(ex)
        end
    end
    unwrap_macrocalls(@nospecialize(x)) = x
    function unwrap_macrocalls(ex::Expr)
        inner = ex
        while inner.head === :macrocall
            inner = inner.args[end]::Expr
        end
        return inner
    end
    is_function_def(@nospecialize(ex)) =
        return Meta.isexpr(ex, :function) || is_short_function_def(ex) || Meta.isexpr(ex, :->)
    function is_short_function_def(@nospecialize(ex))
        Meta.isexpr(ex, :(=)) || return false
        while length(ex.args) >= 1 && isa(ex.args[1], Expr)
            (ex.args[1].head === :call) && return true
            (ex.args[1].head === :where || ex.args[1].head === :(::)) || return false
            ex = ex.args[1]
        end
        return false
    end
end

# https://github.com/JuliaLang/julia/pull/43852
@static if VERSION < v"1.8.0-DEV.1484"
    macro assume_effects(args...)
        esc(last(args))
    end
else
    using Base: @assume_effects
end

if VERSION < v"1.7.0-DEV.119"
    # Part of:
    # https://github.com/JuliaLang/julia/pull/35316
    # https://github.com/JuliaLang/julia/pull/41076
    isunordered(x) = false
    isunordered(x::AbstractFloat) = isnan(x)
    isunordered(x::Missing) = true

    isgreater(x, y) = isunordered(x) || isunordered(y) ? isless(x, y) : isless(y, x)

    Base.findmax(f, domain) = mapfoldl( ((k, v),) -> (f(v), k), _rf_findmax, pairs(domain) )
    _rf_findmax((fm, im), (fx, ix)) = isless(fm, fx) ? (fx, ix) : (fm, im)

    Base.findmin(f, domain) = mapfoldl( ((k, v),) -> (f(v), k), _rf_findmin, pairs(domain) )
    _rf_findmin((fm, im), (fx, ix)) = isgreater(fm, fx) ? (fx, ix) : (fm, im)

    Base.argmax(f, domain) = mapfoldl(x -> (f(x), x), _rf_findmax, domain)[2]
    Base.argmin(f, domain) = mapfoldl(x -> (f(x), x), _rf_findmin, domain)[2]
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

# https://github.com/JuliaLang/julia/pull/41007
if VERSION < v"1.7.0-DEV.1220"
    Base.get(f::Base.Callable, A::AbstractArray, i::Integer) = checkbounds(Bool, A, i) ? A[i] : f()
    Base.get(f::Base.Callable, A::AbstractArray, I::Tuple{}) = checkbounds(Bool, A) ? A[] : f()
    Base.get(f::Base.Callable, A::AbstractArray, I::Dims) = checkbounds(Bool, A, I...) ? A[I...] : f()

    Base.get(t::Tuple, i::Integer, default) = i in 1:length(t) ? getindex(t, i) : default
    Base.get(f::Base.Callable, t::Tuple, i::Integer) = i in 1:length(t) ? getindex(t, i) : f()
end

# https://github.com/JuliaLang/julia/pull/41032
if VERSION < v"1.7.0-DEV.1230"
    Base.get(x::Number, i::Integer, default) = isone(i) ? x : default
    Base.get(x::Number, ind::Tuple, default) = all(isone, ind) ? x : default
    Base.get(f::Base.Callable, x::Number, i::Integer) = isone(i) ? x : f()
    Base.get(f::Base.Callable, x::Number, ind::Tuple) = all(isone, ind) ? x : f()
end

# https://github.com/JuliaLang/julia/pull/29901
if VERSION < v"1.7.0-DEV.1106"
    struct ExceptionStack <: AbstractArray{Any,1}
        stack
    end

    function current_exceptions(task=current_task(); backtrace=true)
        old_stack = Base.catch_stack(task, include_bt=backtrace)
        # If include_bt=true, Base.catch_stack yields a Vector of two-tuples,
        # where the first element of each tuple is an exception and the second
        # element is the corresponding backtrace. If instead include_bt=false,
        # Base.catch_stack yields a Vector of exceptions.
        #
        # Independent of its backtrace keyword argument, Base.current_exceptions
        # yields an ExceptionStack that wraps a Vector of two-element
        # NamedTuples, where the first element of each named tuple is an exception
        # and the second element is either a correpsonding backtrace or `nothing`.
        #
        # The following constructs the ExceptionStack-wrapped Vector appropriately.
        new_stack = backtrace ?
            Any[(exception=exc_and_bt[1], backtrace=exc_and_bt[2]) for exc_and_bt in old_stack] :
            Any[(exception=exc_only,      backtrace=nothing) for exc_only in old_stack]
        return ExceptionStack(new_stack)
    end

    Base.size(s::ExceptionStack) = size(s.stack)
    Base.getindex(s::ExceptionStack, i::Int) = s.stack[i]

    function show_exception_stack(io::IO, stack)
        # Display exception stack with the top of the stack first.  This ordering
        # means that the user doesn't have to scroll up in the REPL to discover the
        # root cause.
        nexc = length(stack)
        for i = nexc:-1:1
            if nexc != i
                printstyled(io, "\ncaused by: ", color=Base.error_color())
            end
            exc, bt = stack[i]
            showerror(io, exc, bt, backtrace = bt!==nothing)
            i == 1 || println(io)
        end
    end

    function Base.display_error(io::IO, stack::ExceptionStack)
        printstyled(io, "ERROR: "; bold=true, color=Base.error_color())
        # Julia >=1.2 provides Base.scrub_repl_backtrace; we use it
        # where possible and otherwise leave backtraces unscrubbed.
        backtrace_scrubber = VERSION >= v"1.2" ? Base.scrub_repl_backtrace : identity
        bt = Any[ (x[1], backtrace_scrubber(x[2])) for x in stack ]
        show_exception_stack(IOContext(io, :limit => true), bt)
        println(io)
    end

    function Base.show(io::IO, ::MIME"text/plain", stack::ExceptionStack)
        nexc = length(stack)
        printstyled(io, nexc, "-element ExceptionStack", nexc == 0 ? "" : ":\n")
        show_exception_stack(io, stack)
    end
    Base.show(io::IO, stack::ExceptionStack) = show(io, MIME("text/plain"), stack)

    export current_exceptions
end

# https://github.com/JuliaLang/julia/pull/39794
if VERSION < v"1.7.0-DEV.793"
    export Returns

    struct Returns{V} <: Function
        value::V
        Returns{V}(value) where {V} = new{V}(value)
        Returns(value) = new{Core.Typeof(value)}(value)
    end

    (obj::Returns)(args...; kw...) = obj.value
    function Base.show(io::IO, obj::Returns)
        show(io, typeof(obj))
        print(io, "(")
        show(io, obj.value)
        print(io, ")")
    end
end

# https://github.com/JuliaLang/julia/pull/39037
if VERSION < v"1.7.0-DEV.204"
    # Borrowed from julia base
    export ismutabletype
    function ismutabletype(@nospecialize(t::Type))
        t = Base.unwrap_unionall(t)
        # TODO: what to do for `Union`?
        return isa(t, DataType) && t.mutable
    end
end

# https://github.com/JuliaLang/julia/pull/42125
if !isdefined(Base, Symbol("@constprop"))
    if isdefined(Base, Symbol("@aggressive_constprop"))
        macro constprop(setting, ex)
            if isa(setting, QuoteNode)
                setting = setting.value
            end
            setting === :aggressive && return esc(:(Base.@aggressive_constprop $ex))
            setting === :none && return esc(ex)
            throw(ArgumentError("@constprop $setting not supported"))
        end
    else
        macro constprop(setting, ex)
            if isa(setting, QuoteNode)
                setting = setting.value
            end
            setting === :aggressive || setting === :none || throw(ArgumentError("@constprop $setting not supported"))
            return esc(ex)
        end
    end
else
    using Base: @constprop
end

# https://github.com/JuliaLang/julia/pull/40803
if VERSION < v"1.8.0-DEV.300"
    function Base.convert(::Type{T}, x::CompoundPeriod) where T<:Period
        return isconcretetype(T) ? sum(T, x.periods) : throw(MethodError(convert, (T, x)))
    end
end

# https://github.com/JuliaLang/julia/pull/39245
if VERSION < v"1.8.0-DEV.487"
    export eachsplit

    """
        eachsplit(str::AbstractString, dlm; limit::Integer=0)
        eachsplit(str::AbstractString; limit::Integer=0)

    Split `str` on occurrences of the delimiter(s) `dlm` and return an iterator over the
    substrings.  `dlm` can be any of the formats allowed by [`findnext`](@ref)'s first argument
    (i.e. as a string, regular expression or a function), or as a single character or collection
    of characters.

    If `dlm` is omitted, it defaults to [`isspace`](@ref).

    The iterator will return a maximum of `limit` results if the keyword argument is supplied.
    The default of `limit=0` implies no maximum.

    See also [`split`](@ref).

    # Examples
    ```julia
    julia> a = "Ma.rch"
    "Ma.rch"
    julia> collect(eachsplit(a, "."))
    2-element Vector{SubString}:
    "Ma"
    "rch"
    ```
    """
    function eachsplit end

    struct SplitIterator{S<:AbstractString,F}
        str::S
        splitter::F
        limit::Int
        keepempty::Bool
    end

    Base.eltype(::Type{<:SplitIterator}) = SubString
    Base.IteratorSize(::Type{<:SplitIterator}) = Base.SizeUnknown()

    function Base.iterate(iter::SplitIterator, (i, k, n)=(firstindex(iter.str), firstindex(iter.str), 0))
        i - 1 > ncodeunits(iter.str)::Int && return nothing
        r = findnext(iter.splitter, iter.str, k)::Union{Nothing,Int,UnitRange{Int}}
        while r !== nothing && n != iter.limit - 1 && first(r) <= ncodeunits(iter.str)
            r = r::Union{Int,UnitRange{Int}} #commit dcc2182db228935fe97d03a44ae3b6889e40c542
            #follow #39245, improve inferrability of iterate(::SplitIterator)
            #Somehow type constraints from the complex `while` condition don't
            #propagate to the `while` body.
            j, k = first(r), nextind(iter.str, last(r))::Int
            k_ = k <= j ? nextind(iter.str, j) : k
            if i < k
                substr = @inbounds SubString(iter.str, i, prevind(iter.str, j)::Int)
                (iter.keepempty || i < j) && return (substr, (k, k_, n + 1))
                i = k
            end
            k = k_
            r = findnext(iter.splitter, iter.str, k)::Union{Nothing,Int,UnitRange{Int}}
        end
        iter.keepempty || i <= ncodeunits(iter.str) || return nothing
        @inbounds SubString(iter.str, i), (ncodeunits(iter.str) + 2, k, n + 1)
    end

    eachsplit(str::T, splitter; limit::Integer=0, keepempty::Bool=true) where {T<:AbstractString} =
        SplitIterator(str, splitter, limit, keepempty)

    eachsplit(str::T, splitter::Union{Tuple{Vararg{AbstractChar}},AbstractVector{<:AbstractChar},Set{<:AbstractChar}};
            limit::Integer=0, keepempty=true) where {T<:AbstractString} =
        eachsplit(str, in(splitter); limit=limit, keepempty=keepempty)

    eachsplit(str::T, splitter::AbstractChar; limit::Integer=0, keepempty=true) where {T<:AbstractString} =
        eachsplit(str, isequal(splitter); limit=limit, keepempty=keepempty)

    eachsplit(str::AbstractString; limit::Integer=0, keepempty=false) =
        eachsplit(str, isspace; limit=limit, keepempty=keepempty)
end

# https://github.com/JuliaLang/julia/pull/43354
if VERSION < v"1.8.0-DEV.1494" # 98e60ffb11ee431e462b092b48a31a1204bd263d
    export allequal
    allequal(itr) = isempty(itr) ? true : all(isequal(first(itr)), itr)
    allequal(c::Union{AbstractSet,AbstractDict}) = length(c) <= 1
    allequal(r::AbstractRange) = iszero(step(r)) || length(r) <= 1
end

# https://github.com/JuliaLang/julia/commit/bdf9ead91e5a8dfd91643a17c1626032faada329
if VERSION < v"1.8.0-DEV.1109"
    # we do not add the methods for == and isless that are included in the above
    # commit, since they are already present in earlier versions.
    import Base: /, rem, mod, lcm, gcd, div
    for op in (:/, :rem, :mod, :lcm, :gcd)
        @eval ($op)(x::Period, y::Period) = ($op)(promote(x, y)...)
    end
    div(x::Period, y::Period, r::RoundingMode) = div(promote(x, y)..., r)
end

# This function is available as of Julia 1.7.
@static if !isdefined(Base, :keepat!)
    export keepat!

    keepat!(B::BitVector, inds) = _keepat!(B, inds)
    keepat!(B::BitVector, inds::AbstractVector{Bool}) = _keepat!(B, inds)
    keepat!(a::Vector, inds) = _keepat!(a, inds)
    keepat!(a::Vector, m::AbstractVector{Bool}) = _keepat!(a, m)

    function _keepat!(a::AbstractVector, inds)
        local prev
        i = firstindex(a)
        for k in inds
            if @isdefined(prev)
                prev < k || throw(ArgumentError("indices must be unique and sorted"))
            end
            ak = a[k] # must happen even when i==k for bounds checking
            if i != k
                @inbounds a[i] = ak # k > i, so a[i] is inbounds
            end
            prev = k
            i = nextind(a, i)
        end
        deleteat!(a, i:lastindex(a))
        return a
    end

    function _keepat!(a::AbstractVector, m::AbstractVector{Bool})
        length(m) == length(a) || throw(BoundsError(a, m))
        j = firstindex(a)
        for i in eachindex(a, m)
            @inbounds begin
                if m[i]
                    i == j || (a[j] = a[i])
                    j = nextind(a, j)
                end
            end
        end
        deleteat!(a, j:lastindex(a))
    end
end

# https://github.com/JuliaLang/julia/pull/43334
if VERSION < v"1.9.0-DEV.1163"
    import Base: IteratorSize, HasLength, HasShape, OneTo
    export stack
    
    """
        stack(iter; [dims])

    Combine a collection of arrays (or other iterable objects) of equal size
    into one larger array, by arranging them along one or more new dimensions.

    By default the axes of the elements are placed first,
    giving `size(result) = (size(first(iter))..., size(iter)...)`.
    This has the same order of elements as [`Iterators.flatten`](@ref)`(iter)`.

    With keyword `dims::Integer`, instead the `i`th element of `iter` becomes the slice
    [`selectdim`](@ref)`(result, dims, i)`, so that `size(result, dims) == length(iter)`.
    In this case `stack` reverses the action of [`eachslice`](@ref) with the same `dims`.

    The various [`cat`](@ref) functions also combine arrays. However, these all
    extend the arrays' existing (possibly trivial) dimensions, rather than placing
    the arrays along new dimensions.
    They also accept arrays as separate arguments, rather than a single collection.

    !!! compat "Julia 1.9"
        This function is available in Julia 1.9, or in Compat 4.2.

    # Examples
    ```jldoctest
    julia> vecs = (1:2, [30, 40], Float32[500, 600]);

    julia> mat = stack(vecs)
    2×3 Matrix{Float32}:
     1.0  30.0  500.0
     2.0  40.0  600.0

    julia> mat == hcat(vecs...) == reduce(hcat, collect(vecs))
    true

    julia> vec(mat) == vcat(vecs...) == reduce(vcat, collect(vecs))
    true

    julia> stack(zip(1:4, 10:99))  # accepts any iterators of iterators
    2×4 Matrix{Int64}:
      1   2   3   4
     10  11  12  13

    julia> vec(ans) == collect(Iterators.flatten(zip(1:4, 10:99)))
    true

    julia> stack(vecs; dims=1)  # unlike any cat function, 1st axis of vecs[1] is 2nd axis of result
    3×2 Matrix{Float32}:
       1.0    2.0
      30.0   40.0
     500.0  600.0

    julia> x = rand(3,4);

    julia> x == stack(eachcol(x)) == stack(eachrow(x), dims=1)  # inverse of eachslice
    true
    ```

    Higher-dimensional examples:

    ```jldoctest
    julia> A = rand(5, 7, 11);

    julia> E = eachslice(A, dims=2);  # a vector of matrices

    julia> (element = size(first(E)), container = size(E))
    (element = (5, 11), container = (7,))

    julia> stack(E) |> size
    (5, 11, 7)

    julia> stack(E) == stack(E; dims=3) == cat(E...; dims=3)
    true

    julia> A == stack(E; dims=2)
    true

    julia> M = (fill(10i+j, 2, 3) for i in 1:5, j in 1:7);

    julia> (element = size(first(M)), container = size(M))
    (element = (2, 3), container = (5, 7))

    julia> stack(M) |> size  # keeps all dimensions
    (2, 3, 5, 7)

    julia> stack(M; dims=1) |> size  # vec(container) along dims=1
    (35, 2, 3)

    julia> hvcat(5, M...) |> size  # hvcat puts matrices next to each other
    (14, 15)
    ```
    """
    stack(iter; dims=:) = _stack(dims, iter)

    """
        stack(f, args...; [dims])

    Apply a function to each element of a collection, and `stack` the result.
    Or to several collections, [`zip`](@ref)ped together.

    The function should return arrays (or tuples, or other iterators) all of the same size.
    These become slices of the result, each separated along `dims` (if given) or by default
    along the last dimensions.

    See also [`mapslices`](@ref), [`eachcol`](@ref).

    # Examples
    ```jldoctest
    julia> stack(c -> (c, c-32), "julia")
    2×5 Matrix{Char}:
     'j'  'u'  'l'  'i'  'a'
     'J'  'U'  'L'  'I'  'A'

    julia> stack(eachrow([1 2 3; 4 5 6]), (10, 100); dims=1) do row, n
             vcat(row, row .* n, row ./ n)
           end
    2×9 Matrix{Float64}:
     1.0  2.0  3.0   10.0   20.0   30.0  0.1   0.2   0.3
     4.0  5.0  6.0  400.0  500.0  600.0  0.04  0.05  0.06
    ```
    """
    stack(f, iter; dims=:) = _stack(dims, f(x) for x in iter)
    stack(f, xs, yzs...; dims=:) = _stack(dims, f(xy...) for xy in zip(xs, yzs...))

    _stack(dims::Union{Integer, Colon}, iter) = _stack(dims, IteratorSize(iter), iter)

    _stack(dims, ::IteratorSize, iter) = _stack(dims, collect(iter))

    function _stack(dims, ::Union{HasShape, HasLength}, iter)
        S = Base.@default_eltype iter
        T = S != Union{} ? eltype(S) : Any  # Union{} occurs for e.g. stack(1,2), postpone the error
        if isconcretetype(T)
            _typed_stack(dims, T, S, iter)
        else  # Need to look inside, but shouldn't run an expensive iterator twice:
            array = iter isa Union{Tuple, AbstractArray} ? iter : collect(iter)
            isempty(array) && return _empty_stack(dims, T, S, iter)
            T2 = mapreduce(eltype, promote_type, array)
            _typed_stack(dims, T2, eltype(array), array)
        end
    end

    function _typed_stack(::Colon, ::Type{T}, ::Type{S}, A, Aax=_iterator_axes(A)) where {T, S}
        xit = iterate(A)
        nothing === xit && return _empty_stack(:, T, S, A)
        x1, _ = xit
        ax1 = _iterator_axes(x1)
        B = similar(_ensure_array(x1), T, ax1..., Aax...)
        off = firstindex(B)
        len = length(x1)
        while xit !== nothing
            x, state = xit
            _stack_size_check(x, ax1)
            copyto!(B, off, x)
            off += len
            xit = iterate(A, state)
        end
        B
    end

    _iterator_axes(x) = _iterator_axes(x, IteratorSize(x))
    _iterator_axes(x, ::HasLength) = (OneTo(length(x)),)
    _iterator_axes(x, ::IteratorSize) = axes(x)

    # For some dims values, stack(A; dims) == stack(vec(A)), and the : path will be faster
    _typed_stack(dims::Integer, ::Type{T}, ::Type{S}, A) where {T,S} =
        _typed_stack(dims, T, S, IteratorSize(S), A)
    _typed_stack(dims::Integer, ::Type{T}, ::Type{S}, ::HasLength, A) where {T,S} =
        _typed_stack(dims, T, S, HasShape{1}(), A)
    function _typed_stack(dims::Integer, ::Type{T}, ::Type{S}, ::HasShape{N}, A) where {T,S,N}
        if dims == N+1
            _typed_stack(:, T, S, A, (_vec_axis(A),))
        else
            _dim_stack(dims, T, S, A)
        end
    end
    _typed_stack(dims::Integer, ::Type{T}, ::Type{S}, ::IteratorSize, A) where {T,S} =
        _dim_stack(dims, T, S, A)

    _vec_axis(A, ax=_iterator_axes(A)) = length(ax) == 1 ? only(ax) : OneTo(prod(length, ax; init=1))

    @constprop :aggressive function _dim_stack(dims::Integer, ::Type{T}, ::Type{S}, A) where {T,S}
        xit = Iterators.peel(A)
        nothing === xit && return _empty_stack(dims, T, S, A)
        x1, xrest = xit
        ax1 = _iterator_axes(x1)
        N1 = length(ax1)+1
        dims in 1:N1 || throw(ArgumentError(string("cannot stack slices ndims(x) = ", N1-1, " along dims = ", dims)))

        newaxis = _vec_axis(A)
        outax = ntuple(d -> d==dims ? newaxis : ax1[d - (d>dims)], N1)
        B = similar(_ensure_array(x1), T, outax...)

        if dims == 1
            _dim_stack!(Val(1), B, x1, xrest)
        elseif dims == 2
            _dim_stack!(Val(2), B, x1, xrest)
        else
            _dim_stack!(Val(dims), B, x1, xrest)
        end
        B
    end

    function _dim_stack!(::Val{dims}, B::AbstractArray, x1, xrest) where {dims}
        before = ntuple(d -> Colon(), dims - 1)
        after = ntuple(d -> Colon(), ndims(B) - dims)

        i = firstindex(B, dims)
        copyto!(view(B, before..., i, after...), x1)

        for x in xrest
            _stack_size_check(x, _iterator_axes(x1))
            i += 1
            @inbounds copyto!(view(B, before..., i, after...), x)
        end
    end

    @inline function _stack_size_check(x, ax1::Tuple)
        if _iterator_axes(x) != ax1
            uax1 = map(UnitRange, ax1)
            uaxN = map(UnitRange, axes(x))
            throw(DimensionMismatch(
                string("stack expects uniform slices, got axes(x) == ", uaxN, " while first had ", uax1)))
        end
    end

    _ensure_array(x::AbstractArray) = x
    _ensure_array(x) = 1:0  # passed to similar, makes stack's output an Array

    _empty_stack(_...) = throw(ArgumentError("`stack` on an empty collection is not allowed"))
end

if v"1.10.0-" <= VERSION < v"1.10.0-DEV.360" || VERSION < v"1.9.0-beta3"
    if VERSION < v"1.9.0-DEV.513"
        # https://github.com/JuliaLang/julia/pull/42717
        export Splat # Base does not export this, but we have to keep it for compatibility

        struct Splat{F} <: Function
            f::F
            Splat(f) = new{Core.Typeof(f)}(f)
        end

        (s::Splat)(args) = s.f(args...)
        Base.print(io::IO, s::Splat) = print(io, "splat(", s.f, ')')
        Base.show(io::IO, s::Splat) = print(io, s)
        Base.show(io::IO, ::MIME"text/plain", s::Splat) = show(io, s)
    end

    # https://github.com/JuliaLang/julia/pull/48038
    export splat
    splat(f) = Splat(f)
end

include("deprecated.jl")

end # module Compat
