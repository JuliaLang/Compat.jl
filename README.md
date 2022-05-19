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

* `keepat!` removes the items at all the indices which are not given and returns
  the modified source ([#36229], [#42351]). (since Compat 4.1.0)

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

[#29901]: https://github.com/JuliaLang/julia/issues/29901
[#35316]: https://github.com/JuliaLang/julia/issues/35316
[#36229]: https://github.com/JuliaLang/julia/issues/36229
[#39037]: https://github.com/JuliaLang/julia/issues/39037
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
[#43354]: https://github.com/JuliaLang/julia/issues/43354
