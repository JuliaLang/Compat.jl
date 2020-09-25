# Compat Package for Julia

[![Build Status](https://travis-ci.org/JuliaLang/Compat.jl.svg?branch=master)](https://travis-ci.org/JuliaLang/Compat.jl)
[![Build status](https://ci.appveyor.com/api/projects/status/github/JuliaLang/Compat.jl?branch=master)](https://ci.appveyor.com/project/quinnj/compat-jl/branch/master)

The **Compat** package is designed to ease interoperability between
older and newer versions of the [Julia
language](http://julialang.org/).  In particular, in cases where it is
impossible to write code that works with both the latest Julia
`master` branch and older Julia versions, or impossible to write code
that doesn't generate a deprecation warning in some Julia version, the
Compat package provides a macro that lets you use the *latest syntax*
in a backwards-compatible way.

This is primarily intended for use by other [Julia
packages](https://julialang.github.io/Pkg.jl/v1/creating-packages/), where
it is important to maintain cross-version compatibility.

## Usage

To use Compat in your Julia package, add it as a dependency of your package using the package manager

```julia
pkg> add Compat
```
and add a [version specifier line](https://julialang.github.io/Pkg.jl/v1/compatibility/#Version-specifier-format-1) such as `Compat = "2.2, 3"` in the `[compat]`section of the `Project.toml` file
in your package directory. The version in the latter should be the minimum
version that supports all needed fatures (see list below), and (if applicable)
any newer major versions verified to be compatible. Then, in your package,
shortly after the `module` statement a line like this:

```julia
using Compat
```

and then as needed add

```julia
@compat ...compat syntax...
```

wherever you want to use syntax that differs in the latest Julia
`master` (the development version of Julia). The `compat syntax` is usually
the syntax on Julia `master`. However, in a few cases where this is not possible,
a slightly different syntax might be used.
Please check the list below for the specific syntax you need.

## Compatibility

Features in the development versions of `julia` may be added and released in
Compat.jl.  However, such features are considered experimental until the
relevant `julia` version is released.  These features can be changed or removed
without incrementing the major version of Compat.jl if necessary to match
changes in `julia`.

## Supported features

* The composition operator `∘` now returns a `Compat.ComposedFunction` instead of an anonymous function ([#37517]). (since Compat 3.17)

* New function `addenv` for adding environment mappings into a `Cmd` object, returning the new `Cmd` object ([#37244]). (since Compat 3.16)

* `contains(haystack, needle)` and its one argument partially applied form `contains(haystack)` have been added, it acts like `occursin(needle, haystack)` ([#35132]). (since Compat 3.15)

* `startswith(x)` and `endswith(x)`, returning partially-applied versions of the functions, similar to existing methods like `isequal(x)` ([#35052]). (since Compat 3.15)

* `Compat.Iterators.map` is added. It provides another syntax `Iterators.map(f, iterators...)`
  for writing `(f(args...) for args in zip(iterators...))`, i.e. a lazy `map`  ([#34352]).
  (since Compat 3.14)

* `takewhle` and `dropwhile` are added in `Compat.Iterators` ([#33437]). (since Compat 3.14)

* Curried comparisons `!=(x)`, `>=(x)`, `<=(x)`, `>(x)`, and `<(x)` are defined as, e.g.,
  `!=(x) = y -> y != x` ([#30915]). (since Compat 3.14)

* `strides` is defined for Adjoint and Transpose ([#35929]). (since Compat 3.14)

* `Compat.get_num_threads()` adds the functionality of `LinearAlgebra.BLAS.get_num_threads()`, and has matching `Compat.set_num_threads(n)` ([#36360]). (since Compat 3.13.0)

* `@inferred [AllowedType] f(x)` is defined ([#27516]). (since Compat 3.12.0)

* Search a character in a string with `findfirst`, `findnext`, `findlast` and `findprev` ([#31664]). (since Compat 3.12.0)

* `∘(f) = f` is defined ([#34251]). (since Compat 3.11.0)

* `union` supports `Base.OneTo` ([#35577]). (since Compat 3.11.0)

* `get` supports `CartesianIndex` when indexing `AbstractArrays` ([#30268]). (since Compat 3.10.0)

* `similar(::PermutedDimsArray)` now uses the parent ([#35304]). (since Compat 3.9.0)

* `isdisjoint(l, r)` indicates whether two collections are disjoint ([#34427]). (since Compat 3.9.0)

* `mergewith(combine, dicts...)` and `mergewith!(combine, dicts...)` are like
  `merge(combine, dicts...)` and `merge!(combine, dicts...)` but without the restriction
  that the argument `combine` must be a `Function` ([#34296]). (since Compat 3.9.0).

* `@NamedTuple` macro for convenient `struct`-like syntax for declaring
`NamedTuple` types via `key::Type` declarations ([#34548]). (since Compat 3.8.0)

* `evalpoly(x, (p1, p2, ...))`, the function equivalent to `@evalpoly(x, p1, p2, ...)`
  ([#32753]). (since Compat 3.7.0)

* `zero(::Irrational)` and `one` now defined ([#34773]). (since Compat 3.6.0)

* `I1:I2`, when `I1` and `I2` are CartesianIndex values, constructs a CartesianIndices
  iterator ([#29440]). (Since Compat 3.5.0)

* `oneunit(::CartesianIndex)` replaces `one(::CartesianIndex)` ([#29442]). (Since Compat 3.5.0)

* `ismutable` return `true` iff value `v` is mutable ([#34652]). (since Compat 3.4.0)

* `uuid5` generates a version 5 universally unique identifier (UUID), as specified by RFC 4122 ([#28761]). (since Compat 3.3.0)

* `dot` now has a 3-argument method `dot(x, A, y)` without storing the intermediate result `A*y` ([#32739]). (since Compat 3.2.0)

* `pkgdir(m)` returns the root directory of the package that imported module `m` ([#33128]). (since Compat 3.2.0)

* `filter` can now act on a `Tuple` [#32968]. (since Compat 3.1.0)

* `Base.Order.ReverseOrdering` has a zero arg constructor [#33736]. (since Compat 3.0.0)

* Function composition now supports multiple functions: `∘(f, g, h) = f ∘ g ∘ h`
  and splatting `∘(fs...)` for composing an iterable collection of functions ([#33568]).  (since Compat 3.0.0)

* `only(x)` returns the one-and-only element of a collection `x` ([#33129]). (since Compat 2.2.0)

* `mod` now accepts a unit range as the second argument ([#32628]). (since Compat 2.2.0)

* `eachrow`, `eachcol`, and `eachslice` to iterate over first, second, or given dimension
  of an array ([#29749]). (since Compat 2.2.0)

* `isnothing` for testing if a variable is equal to `nothing` ([#29674]).  (since Compat 2.1.0)

* `hasproperty` and `hasfield` ([#28850]).  (since Compat 2.0.0)

* `merge` methods with one and `n` `NamedTuple`s ([#29259]). (since Compat 2.0.0)

* `range` supporting `stop` as positional argument ([#28708]). (since Compat 1.3.0)

## Developer tips

One of the most important rules for `Compat.jl` is to avoid breaking user code
whenever possible, especially on a released version.

Although the syntax used in the most recent Julia version
is the preferred compat syntax, there are cases where this shouldn't be used.
Examples include when the new syntax already has a different meaning
on previous versions of Julia, or when functions are removed from `Base`
Julia and the alternative cannot be easily implemented on previous versions.
In such cases, possible solutions are forcing the new feature to be used with
qualified name in `Compat.jl` (e.g. use `Compat.<name>`) or
reimplementing the old features on a later Julia version.

If you're adding additional compatibility code to this package, the [`contrib/commit-name.sh`](https://github.com/JuliaLang/julia/blob/master/contrib/commit-name.sh) script in the base Julia repository is useful for extracting the version number from a git commit SHA. For example, from the git repository of `julia`, run something like this:

```sh
bash $ contrib/commit-name.sh a378b60fe483130d0d30206deb8ba662e93944da
0.5.0-dev+2023
```

This prints a version number corresponding to the specified commit of the form
`X.Y.Z-aaa+NNNN`, and you can then test whether Julia
is at least this version by `VERSION >= v"X.Y.Z-aaa+NNNN"`.

### Tagging the correct minimum version of Compat

Note that you should specify the correct minimum version for `Compat` in the
`[compat]` section of your `Project.toml`, as given in above list.

[#28708]: https://github.com/JuliaLang/julia/issues/28708
[#28761]: https://github.com/JuliaLang/julia/issues/28761
[#28850]: https://github.com/JuliaLang/julia/issues/28850
[#29259]: https://github.com/JuliaLang/julia/issues/29259
[#29440]: https://github.com/JuliaLang/julia/issues/29440
[#29442]: https://github.com/JuliaLang/julia/issues/29442
[#29674]: https://github.com/JuliaLang/julia/issues/29674
[#29749]: https://github.com/JuliaLang/julia/issues/29749
[#31664]: https://github.com/JuliaLang/julia/issues/31664
[#32628]: https://github.com/JuliaLang/julia/issues/32628
[#32739]: https://github.com/JuliaLang/julia/issues/32739
[#32753]: https://github.com/JuliaLang/julia/issues/32753
[#32968]: https://github.com/JuliaLang/julia/issues/32968
[#33128]: https://github.com/JuliaLang/julia/issues/33128
[#33129]: https://github.com/JuliaLang/julia/issues/33129
[#33568]: https://github.com/JuliaLang/julia/issues/33568
[#33736]: https://github.com/JuliaLang/julia/issues/33736
[#34296]: https://github.com/JuliaLang/julia/issues/34296
[#34427]: https://github.com/JuliaLang/julia/issues/34427
[#34548]: https://github.com/JuliaLang/julia/issues/34548
[#34652]: https://github.com/JuliaLang/julia/issues/34652
[#34773]: https://github.com/JuliaLang/julia/issues/34773
[#35304]: https://github.com/JuliaLang/julia/pull/35304
[#30268]: https://github.com/JuliaLang/julia/pull/30268
[#34251]: https://github.com/JuliaLang/julia/pull/34251
[#35577]: https://github.com/JuliaLang/julia/pull/35577
[#27516]: https://github.com/JuliaLang/julia/pull/27516
[#36360]: https://github.com/JuliaLang/julia/pull/36360
[#35929]: https://github.com/JuliaLang/julia/pull/35929
[#30915]: https://github.com/JuliaLang/julia/pull/30915
[#33437]: https://github.com/JuliaLang/julia/pull/33437
[#34352]: https://github.com/JuliaLang/julia/pull/34352
[#35132]: https://github.com/JuliaLang/julia/pull/35132
[#35052]: https://github.com/JuliaLang/julia/pull/35052
[#37244]: https://github.com/JuliaLang/julia/pull/37244
[#37517]: https://github.com/JuliaLang/julia/pull/37517
