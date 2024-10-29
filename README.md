# Compat Package for Julia

[![Build Status](https://github.com/JuliaLang/Compat.jl/actions/workflows/CI.yml/badge.svg)](https://github.com/JuliaLang/Compat.jl/actions/workflows/CI.yml)

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
and add a [version specifier line](https://julialang.github.io/Pkg.jl/v1/compatibility/#Version-specifier-format-1)
such as `Compat = "3.22, 4"` in the `[compat]`section of the `Project.toml` file
in your package directory. The version in the latter should be the minimum
version that supports all needed features (see list below). Note that Compat v5
requires Julia v1.10, but some features may have been backported to Compat v4
(see the
[feature list of the release-4 branch](https://github.com/JuliaLang/Compat.jl/tree/release-4#supported-features)).
If you require any of those backported features, be sure to specify the correct
compatibility in your `Project.toml`. E.g. if the feature from Compat v5.x has
been backported to v4.y, use `Compat = 4.y, 5.x`. If you use a feature that had
originally been added in Compat v4 (e.g. in 4.x), don't forget to also declare
compatibility with v5 with `Compat = 4.x, 5`.

To minimize dependency conflicts between packages it is recommended that packages
allow for both appropriate v5 and v4 versions of Compat.jl in their Project.toml
(except for rare cases of packages that support only v5 or v4 version of Compat.jl).

Then, in your package, shortly after the `module` statement include a line like
this:

```julia
using Compat
```

and then as needed add

```julia
@compat ...compat syntax...
```

wherever you want to use _syntax_ that differs in the latest Julia
`master` (the development version of Julia). The `compat syntax` is usually
the syntax on Julia `master`. However, in a few cases where this is not possible,
a slightly different syntax might be used.
Please check the list below for the specific syntax you need.

Note that if you want to use a _function_ `new_f` defined in later Julia versions, it is redefined in Compat (as `Compat.new_f`) and exported, so you don't need to change anything to your code beyond the initial import. 

## Compatibility

Features in the development versions of `julia` may be added and released in
Compat.jl.  However, such features are considered experimental until the
relevant `julia` version is released.  These features can be changed or removed
without incrementing the major version of Compat.jl if necessary to match
changes in `julia`.

## Supported features

* `insertdims(D; dims)` is the opposite of `dropdims` ([#45793]) (since Compat 4.16.0)

* `Compat.Fix{N}` which fixes an argument at the `N`th position ([#54653]) (since Compat 4.16.0)

* `logrange(lo, hi; length)` is like `range` but with a constant ratio, not difference. ([#39071]) (since Compat 4.14.0)

* `allequal(f, itr)` and `allunique(f, itr)` methods. ([#47679]) (since Compat 4.13.0)

* `Iterators.cycle(itr, n)` is the lazy version of `repeat(vector, n)`. ([#47354]) (since Compat 4.13.0)

* `@compat public foo, bar` marks `foo` and `bar` as public in Julia 1.11+ and is a no-op in Julia 1.10 and earlier. ([#50105]) (since Compat 3.47.0, 4.10.0)

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

[`@bdf9ead9`]: https://github.com/JuliaLang/julia/commit/bdf9ead91e5a8dfd91643a17c1626032faada329
[#39071]: https://github.com/JuliaLang/julia/issues/39071
[#45793]: https://github.com/JuliaLang/julia/issues/45793
[#47354]: https://github.com/JuliaLang/julia/issues/47354
[#47679]: https://github.com/JuliaLang/julia/issues/47679
[#50105]: https://github.com/JuliaLang/julia/issues/50105
[#54653]: https://github.com/JuliaLang/julia/issues/54653
