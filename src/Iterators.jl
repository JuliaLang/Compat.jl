module Iterators

using Base.Iterators
cycle(xs, n::Integer) = flatten(repeated(xs, n))
cycle(args...) = Base.Iterators.cycle(args...)

end
