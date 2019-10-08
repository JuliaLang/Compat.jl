Base.@deprecate_binding StringVector Base.StringVector false

# PR #17302
# Provide a non-deprecated version of `@vectorize_(1|2)arg` macro which defines
# deprecated version of the function so that the depwarns can be fixed without
# breaking users.
# Packages are expected to use this to maintain the old API until all users
# of the deprecated vectorized function have migrated.
# These macros should be dropped when the support for `0.6` is dropped from `Compat`.
# Modified based on the version copied from 0.6 Base.
if VERSION < v"0.7.0-DEV.1211"
    macro dep_vectorize_1arg(S, f)
        S = esc(S)
        f = esc(f)
        T = esc(:T)
        x = esc(:x)
        AbsArr = esc(:AbstractArray)
        Base.depwarn("Implicit vectorized function is deprecated in favor of compact broadcast syntax.",
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
        Base.depwarn("Implicit vectorized function is deprecated in favor of compact broadcast syntax.",
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
        Base.depwarn("Implicit vectorized function is deprecated in favor of compact broadcast syntax.",
            Symbol("@dep_vectorize_1arg"))
        return esc(:(@deprecate $f(x::$AbstractArray{T}) where {T<:$S} $f.(x)))
    end

    macro dep_vectorize_2arg(S, f)
        AbstractArray = GlobalRef(Base, :AbstractArray)
        Base.depwarn("Implicit vectorized function is deprecated in favor of compact broadcast syntax.",
            Symbol("@dep_vectorize_2arg"))
        return esc(quote
            @deprecate $f(x::$S, y::$AbstractArray{T1}) where {T1<:$S} $f.(x, y)
            @deprecate $f(x::$AbstractArray{T1}, y::$S) where {T1<:$S} $f.(x, y)
            @deprecate $f(x::$AbstractArray{T1}, y::$AbstractArray{T2}) where {T1<:$S, T2<:$S} $f.(x, y)
        end)
    end
end

# compatibility with https://github.com/JuliaLang/julia/pull/26156
Base.@deprecate trunc(x, digits; base = 10) Compat.trunc(x, digits = digits, base = base) false
Base.@deprecate floor(x, digits; base = 10) Compat.floor(x, digits = digits, base = base) false
Base.@deprecate ceil(x, digits; base = 10) Compat.ceil(x, digits = digits, base = base) false
Base.@deprecate round(x, digits; base = 10) Compat.round(x, digits = digits, base = base) false
Base.@deprecate signif(x, digits; base = 10) Compat.round(x, sigdigits = digits, base = base) false
# standard methods from Base; can be removed (so that Compat just inherits the functions
# from Base) once above deprecations are removed
trunc(x; digits = 0, base = 10) = Base.trunc(x, digits = digits, base = base)
floor(x; digits = 0, base = 10) = Base.floor(x, digits = digits, base = base)
ceil(x; digits = 0, base = 10) = Base.ceil(x, digits = digits, base = base)
round(x; digits = nothing, sigdigits = nothing, base = 10) = Base.round(x, digits = digits, sigdigits = sigdigits, base = base)

if VERSION >= v"1.1.0-DEV.506"
    # deprecation of range(start, stop) for earlier versions is done in Compat.jl
    # This method is restricted to Number, since we don't
    # want to overwrite the (::Any, ::Any) method in Base.
    function Base.range(start::Number, stop::Number; kwargs...)
        rangedepwarn(;kwargs...)
        range(start; stop=stop, kwargs...)
    end
end

# this was defined for use with Julia versions prior to 0.5
# (see https://github.com/JuliaLang/Compat.jl/pull/316)
macro dotcompat(x)
    Base.depwarn("`@dotcompat x` is deprecated, use `@compat @. x` instead.", Symbol("@dotcompat"))
    esc(:(Compat.@compat @. $x))
end
export @dotcompat

# Compat.Random.uuid1, uuid4, uuid_version are deprecated in Compat.jl
