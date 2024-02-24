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
version that supports all needed features (see list below). Note that Compat v4
requires Julia v1.6, but some features may have been backported to Compat v3
(see the
[feature list of the release-3 branch](https://github.com/JuliaLang/Compat.jl/tree/release-3#supported-features)).
If you require any of those backported features, be sure to specify the correct
compatibility in your `Project.toml`. E.g. if the feature from Compat v4.x has
been backported to v3.y, use `Compat = 3.y, 4.x`. If you use a feature that had
originally been added in Compat v3 (e.g. in 3.x), don't forget to also declare
compatibility with v4 with `Compat = 3.x, 4` (unless you use one the very few
[things that got removed between Compat v3 and v4](https://github.com/JuliaLang/Compat.jl/releases/tag/v4.0.0),
which you most probably don't).

To minimize dependency conflicts between packages it is recommended that packages
allow for both appropriate v4 and v3 versions of Compat.jl in their Project.toml
(except for rare cases of packages that support only v4 or v3 version of Compat.jl).

Then, in your package, shortly after the `module` statement include a line like
this:

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

* `allequal(f, itr)` and `allunique(f, itr)` methods. ([#47679]) (since Compat 4.13.0)

* `logrange(lo, hi; length)` is like `range` but with a constant ratio, not difference. ([#39071]) (since Compat 4.14.0) Note that on Julia 1.8 and earlier, the version from Compat has slightly lower floating-point accuracy than the one in Base (Julia 1.11 and later).

* `Iterators.cycle(itr, n)` is the lazy version of `repeat(vector, n)`. ([#47354]) (since Compat 4.13.0)

* `@compat public foo, bar` marks `foo` and `bar` as public in Julia 1.11+ and is a no-op in Julia 1.10 and earlier. ([#50105]) (since Compat 3.47.0, 4.10.0)

* `redirect_stdio`, for simple stream redirection. ([#37978]) (since Compat 4.8.0)

* `trunc`, `floor`, `ceil`, and `round` to `Bool`. ([#25085]) (since Compat 4.7.0)

* `splat(f)` which is equivalent to `args -> f(args...)`. ([#42717], [#48038]) (since Compat 4.6.0) (Note: for historical reasons, `Compat` on Julia before v1.9 also exports `Splat`; its usage is discouraged, however.)

* `Compat.@assume_effects setting... ex` overrides the compiler's effect modeling for the method definition `ex` on Julia versions that support this feature. Julia version without support just pass back `ex`. ([#43852]) (since Compat 4.4.0)

* `div`, `lcm`, `gcd`, `/`, `rem`, and `mod` will `promote` heterogenous `Dates.Period`s ([`@bdf9ead9`]). (since Compat 4.3.0)

* `stack` combines a collection of slices into one array ([#43334]). (since Compat 3.46.0, 4.2.0)

* `keepat!` removes the items at all the indices which are not given and returns
  the modified source ([#36229], [#42351]). (since Compat 3.44.0, 4.1.0)

* `@compat (; a, b) = (; c=1, b=2, a=3)` supports property descturing assignment syntax ([#39285]).

* `allequal`, the opposite of `allunique` ([#43354]). (since Compat 3.42.0)

* `eachsplit` for iteratively performing split(str). ([#39245]). (since Compat 3.41.0)

* `ismutabletype(t::Type)` check whether a type is mutable (the field `mutable` of `DataType` was removed. [#39037]) (since Compat 3.40)

* `convert(::Type{<:Period}, ::CompoundPeriod)` can convert `CompoundPeriod`s into the specified `Period` type ([#40803]) (since Compat 3.38.0)

* `Compat.@inline` and `Compat.@noinline` can be used at function callsites to encourage the compiler to (not) inline the function calls on Julia versions that support these features, and otherwise do not have any effects ([#41312]) (since Compat 3.37)

* `Compat.@inline` and `Compat.@noinline` can be used within function body to hint to the compiler the inlineability of the defined function ([#41312]) (since Compat 3.37)

* `Compat.@constprop :aggressive ex` and `Compat.@constprop :none ex` allow control over constant-propagation during inference on Julia versions that support this feature, and otherwise just pass back `ex`. ([#42125]) (since Compat 3.36)

* `Returns(value)` returns `value` for any arguments ([#39794]) (since Compat 3.35)

* The function `current_exceptions()` has been added to get the current
  exception stack. Julia-1.0 lacks runtime support for full execption stacks,
  so we return only the most recent exception in that case. ([#29901]) (since
  Compat 3.34)

* Two argument methods `findmax(f, domain)`, `argmax(f, domain)` and the corresponding `min` versions ([#35316], [#41076]) (since Compat 3.31.1)

* `isunordered(x)` returns true if `x` is value that is normally unordered, such as `NaN` or `missing` ([#35316]) (since Compat 3.31.1)

* `get` accepts tuples and numbers ([#41007], [#41032]) (since Compat 3.31)

* `@something` and `@coalesce` as short-circuiting versions of `something` and `coalesce` ([#40729]) (since Compat 3.29)

* `pkgversion(m::Module)` returns the version of the package that loaded a given module ([#45607]) (since Compat 4.11)

* `VersionNumber(::VersionNumber)` defined as a no-op constructor ([#45052]) (since Compat 4.12)

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
[#25085]: https://github.com/JuliaLang/julia/issues/25085
[#29901]: https://github.com/JuliaLang/julia/issues/29901
[#35316]: https://github.com/JuliaLang/julia/issues/35316
[#36229]: https://github.com/JuliaLang/julia/issues/36229
[#37978]: https://github.com/JuliaLang/julia/issues/37978
[#39037]: https://github.com/JuliaLang/julia/issues/39037
[#39071]: https://github.com/JuliaLang/julia/pull/39071
[#39245]: https://github.com/JuliaLang/julia/issues/39245
[#39285]: https://github.com/JuliaLang/julia/issues/39285
[#39794]: https://github.com/JuliaLang/julia/issues/39794
[#40729]: https://github.com/JuliaLang/julia/issues/40729
[#40803]: https://github.com/JuliaLang/julia/issues/40803
[#41007]: https://github.com/JuliaLang/julia/issues/41007
[#41032]: https://github.com/JuliaLang/julia/issues/41032
[#41076]: https://github.com/JuliaLang/julia/issues/41076
[#41312]: https://github.com/JuliaLang/julia/issues/41312
[#42125]: https://github.com/JuliaLang/julia/issues/42125
[#42351]: https://github.com/JuliaLang/julia/issues/42351
[#42717]: https://github.com/JuliaLang/julia/issues/42717
[#43334]: https://github.com/JuliaLang/julia/issues/43334
[#43354]: https://github.com/JuliaLang/julia/issues/43354
[#43852]: https://github.com/JuliaLang/julia/issues/43852
[#45052]: https://github.com/JuliaLang/julia/issues/45052
[#45607]: https://github.com/JuliaLang/julia/issues/45607
[#47354]: https://github.com/JuliaLang/julia/issues/47354
[#48038]: https://github.com/JuliaLang/julia/issues/48038
[#50105]: https://github.com/JuliaLang/julia/issues/50105
[#47679]: https://github.com/JuliaLang/julia/pull/47679
