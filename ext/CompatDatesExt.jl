module CompatDatesExt
using Dates
using Dates: Period, CompoundPeriod

# https://github.com/JuliaLang/julia/pull/40803
if VERSION < v"1.8.0-DEV.300"
    function Base.convert(::Type{T}, x::CompoundPeriod) where T<:Period
        return isconcretetype(T) ? sum(T, x.periods) : throw(MethodError(convert, (T, x)))
    end
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
end
