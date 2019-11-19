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

To use Compat in your Julia package, add a line
`Compat = "34da2185-b29b-5c13-b0c7-acf172513d20"` in the `[deps]` section
and a line `Compat = "..."` in the `[compat]`section to the `Project.toml` file
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

## Supported syntax

Currently, the `@compat` macro supports the following syntaxes:

## Module Aliases

## New functions, macros, and methods

* `pkgdir(m)` return the root directory of the package that imported module `m` ([#33128]). (since Compat 2.3.0)

* `only(x)` returns the one-and-only element of a collection `x` ([#33129]). (since Compat 2.2.0)

* `mod` now accepts a unit range as the second argument ([#32628]). (since Compat 2.2.0)

* `eachrow`, `eachcol`, and `eachslice` to iterate over first, second, or given dimension
  of an array ([#29749]). (since Compat 2.2.0)

* `isnothing` for testing if a variable is equal to `nothing` ([#29674]).  (since Compat 2.1.0)

* `range` supporting `stop` as positional argument ([#28708]). (since Compat 1.3.0)

* `hasproperty` and `hasfield` ([#28850]).  (since Compat 2.0.0)

* `merge` methods with one and `n` `NamedTuple`s ([#29259]). (since Compat 2.0.0)

* `Base.Order.ReverseOrdering` has a zero arg constructor [#33736]. (since Compat 3.0.0)

## Renaming

## New macros

## Other changes

* Function composition now supports multiple functions: `∘(f, g, h) = f ∘ g ∘ h`
  and splatting `∘(fs...)` for composing an iterable collection of functions ([#33568]).  (since Compat 3.0.0)

## New types

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
[#28850]: https://github.com/JuliaLang/julia/issues/28850
[#29259]: https://github.com/JuliaLang/julia/issues/29259
[#29674]: https://github.com/JuliaLang/julia/issues/29674
[#29749]: https://github.com/JuliaLang/julia/issues/29749
[#32628]: https://github.com/JuliaLang/julia/issues/32628
[#33129]: https://github.com/JuliaLang/julia/issues/33129
[#33568]: https://github.com/JuliaLang/julia/pull/33568
[#33128]: https://github.com/JuliaLang/julia/pull/33128
[#33736]: http://github.com/JuliaLang/julia/pull/33736
