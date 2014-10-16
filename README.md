# Compatibility

[![Build Status](https://travis-ci.org/JuliaLang/Compat.jl.svg?branch=master)](https://travis-ci.org/JuliaLang/Compat.jl)

There is some syntax breakage between Julia v0.3 and v0.4, and there will be more, it's increasingly tricky to make packages to work on both versions. 
The Compat package was just created to help: it provides compatibility constructs that will work in both versions without warnings.

For example, in v0.3 you could create a dictionary like this:

julia> [ :foo => 1, :bar => 2 ]
Dict{Symbol,Int64} with 2 entries:
  :bar => 2
  :foo => 1

This still works in v0.4 but it produces a warning. The new syntax is this:

julia> Dict(:foo => 1, :bar => 2)
Dict{Symbol,Int64} with 2 entries:
  :bar => 2
  :foo => 1

However, this newer syntax won't work in v0.3, so you're a bit stuck if you want to write a dictionary literal in a way that will work in both v0.3 and v0.4 without producing a warning. Compat to the rescue!:

julia> using Compat

julia> @Compat.Dict(:foo => 2, :bar => 2)
Dict{Symbol,Int64} with 2 entries:
  :bar => 2
  :foo => 2

This works with no warning on both v0.3 and v0.4. We've intentionally not exported the Dict macro so that the usage needs to be prefixed with "Compat.", which will make usages of the compatibility workarounds easier to find and remove later when they're no longer necessary.

Currently, there's only a couple of definitions in the Compat package, but if you have your own hacks that have helped make it easier to write cross-version package code, please contribute them and we can build up a nice little collection.
