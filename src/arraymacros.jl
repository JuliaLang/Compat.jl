# TODO deprecate
# this was defined for use with Julia versions prior to 0.5
# (see https://github.com/JuliaLang/Compat.jl/pull/316)
macro dotcompat(x)
    esc(_compat(Base.Broadcast.__dot__(x)))
end
export @dotcompat
