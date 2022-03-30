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

[#39037]: https://github.com/JuliaLang/julia/pull/39037
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
[#34595]: https://github.com/JuliaLang/julia/pull/34595
[#35132]: https://github.com/JuliaLang/julia/pull/35132
[#35052]: https://github.com/JuliaLang/julia/pull/35052
[#37244]: https://github.com/JuliaLang/julia/pull/37244
[#37517]: https://github.com/JuliaLang/julia/pull/37517
[#37559]: https://github.com/JuliaLang/julia/pull/37559
[#29634]: https://github.com/JuliaLang/julia/pull/29634
[#35243]: https://github.com/JuliaLang/julia/pull/35243
[#37396]: https://github.com/JuliaLang/julia/pull/37396
[#37391]: https://github.com/JuliaLang/julia/pull/37391
[#35816]: https://github.com/JuliaLang/julia/pull/35816
[#29790]: https://github.com/JuliaLang/julia/pull/29790
[#38449]: https://github.com/JuliaLang/julia/pull/38449
[#36018]: https://github.com/JuliaLang/julia/pull/36018
[#36199]: https://github.com/JuliaLang/julia/pull/36199
[#37454]: https://github.com/JuliaLang/julia/pull/37454
[#40729]: https://github.com/JuliaLang/julia/pull/40729
[#37065]: https://github.com/JuliaLang/julia/pull/37065
[#41007]: https://github.com/JuliaLang/julia/pull/41007
[#41032]: https://github.com/JuliaLang/julia/pull/41032
[#35316]: https://github.com/JuliaLang/julia/pull/35316
[#41076]: https://github.com/JuliaLang/julia/pull/41076
[#34331]: https://github.com/JuliaLang/julia/pull/34331
[#39794]: https://github.com/JuliaLang/julia/pull/39794
[#40803]: https://github.com/JuliaLang/julia/pull/40803
[#42125]: https://github.com/JuliaLang/julia/pull/42125
[#41312]: https://github.com/JuliaLang/julia/pull/41312
[#41328]: https://github.com/JuliaLang/julia/pull/41328
[#43354]: https://github.com/JuliaLang/julia/pull/43354
[#39285]: https://github.com/JuliaLang/julia/pull/39285
