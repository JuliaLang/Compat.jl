const var"@assume_effects" = getglobal(Base, Symbol("@assume_effects"))
Base.deprecate(@__MODULE__, Symbol("@assume_effects"))

const var"@constprop" = getglobal(Base, Symbol("@constprop"))
Base.deprecate(@__MODULE__, Symbol("@constprop"))

if VERSION < v"1.11.0-DEV.1562"
    @deprecate Base.allunique(f, xs) Compat.allunique(f, xs) false
    @deprecate Base.allequal(f, xs) Compat.allequal(f, xs) false
end

if VERSION < v"1.11.0-DEV.1579"
    @deprecate Base.Iterators.cycle(xs, n::Integer) Compat.Iterators.cycle(xs, n::Integer) false
end
