# Compat Package for Julia

[![Build Status](https://travis-ci.org/JuliaLang/Compat.jl.svg?branch=master)](https://travis-ci.org/JuliaLang/Compat.jl)
[![Build status](https://ci.appveyor.com/api/projects/status/github/JuliaLang/Compat.jl?branch=master)](https://ci.appveyor.com/project/quinnj/compat-jl/branch/master)

[![Compat](http://pkg.julialang.org/badges/Compat_0.3.svg)](http://pkg.julialang.org/?pkg=Compat&ver=0.3)
[![Compat](http://pkg.julialang.org/badges/Compat_0.4.svg)](http://pkg.julialang.org/?pkg=Compat&ver=0.4)
[![Compat](http://pkg.julialang.org/badges/Compat_0.5.svg)](http://pkg.julialang.org/?pkg=Compat&ver=0.5)
[![Compat](http://pkg.julialang.org/badges/Compat_0.6.svg)](http://pkg.julialang.org/?pkg=Compat&ver=0.6)
[![Compat](http://pkg.julialang.org/badges/Compat_0.7.svg)](http://pkg.julialang.org/?pkg=Compat&ver=0.7)

The **Compat** package is designed to ease interoperability between
older and newer versions of the [Julia
language](http://julialang.org/).  In particular, in cases where it is
impossible to write code that works with both the latest Julia
`master` branch and older Julia versions, or impossible to write code
that doesn't generate a deprecation warning in some Julia version, the
Compat package provides a macro that lets you use the *latest syntax*
in a backwards-compatible way.

This is primarily intended for use by other [Julia
packages](http://docs.julialang.org/en/latest/manual/packages/), where
it is important to maintain cross-version compatibility.

## Usage

To use Compat in your Julia package, add a line `Compat` to the
`REQUIRE` file in your package directory.  Then, in your package,
shortly after the `module` statement include lines like these:

```julia
using Compat
import Compat.String
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

* `@compat finalizer(func, obj)` with the finalizer to run as the first argument
  and the object to be finalized as the second ([#24605]).

* `@compat foo(::CartesianRange{N})` to replace the former
  `foo(::CartesianRange{CartesianIndex{N}})` ([#20974]). Note that
  `CartesianRange` now has two type parameters, so using them as
  fields in other `struct`s requires manual intervention.

## Module Aliases

* `using Compat.Test`, `using Compat.SharedArrays`, `using Compat.Mmap`, and `using
  Compat.DelimitedFiles` are provided on versions older than 0.7, where these are not yet
  part of the standard library. ([#23931])

* `using Compat.Base64` is provided on versions older than 0.7, where this library is not
  yet a part of the standard library. ([#24361])

* `using Compat.Dates` is provided on versions older than 0.7, where this library is not
  yet a part of the standard library. ([#24459])

* `using Compat.Unicode` is provided on versions older than 0.7, where this library is not
  yet a part of the standard library. ([#25021])

* `using Compat.Printf` is provided on versions older than 0.7, where this library is not
  yet a part of the standard library. ([#25056])

* `using Compat.IterativeEigensolvers` is provided on versions older than 0.7, where this
  library is not yet a part of the standard library. ([#24714])

* `using Compat.SuiteSparse` is provided on versions older than 0.7, where this library is
  not yet part of the standard library ([#24648]).

## New functions, macros, and methods

* `Compat.invokelatest` supports keyword arguments on Julia version 0.6 ([#22646]).

* `@__MODULE__` is aliased to `current_module()` for Julia version 0.6. Versions of `Base.binding_module`, `expand`, `macroexpand`, and `include_string` were added that accept a module as the first argument. ([#22064])

* `Cmd` elements can be accessed as if the `Cmd` were an array of strings for 0.6 ([#21197]).

* `Val(x)` constructs `Val{x}()`. ([#22475])

* The `reshape` and `ntuple` APIs are extended to support `Val{x}()` arguments on 0.6.

* `chol` and `chol!` for `UniformScalings` ([#22633]).

* `logdet` for `Number`s ([#22629]).

* `fieldcount` is equivalent to `nfields` for Julia versions 0.6 and is used to
  determine the number of fields in a data type ([#22350]).

* There are versions of `InexactError`, `DomainError`, and `OverflowError` that take the same arguments as introduced in Julia 0.7-DEV ([#20005], [#22751], [#22761]).

* `Base.rtoldefault` now takes a third parameter `atol`.
  The two argument form is deprecated in favor of the three arguments form with `atol=0`.

* The `corrected` optional argument of `cov` becomes a keyword argument ([#21709]).

* `equalto` constructs an `EqualTo` object that can be used as a predicate ([#23812]).

* `*(::Union{Char,AbstractString},::Union{Char,AbstractString})` concatenation. ([#22512])

* `diagm` and `spdiagm` accept pairs mapping diagonals to vectors ([#24047], [#23757])

* Constructors for `Matrix{T}`, `Array{T}`, and `SparseMatrixCSC{T}` from `UniformScaling` ([#24372], [#24657])

* `Uninitialized` and `uninitialized` with corresponding `Array` constructors ([#24652]).

* `BitArray` constructors for `uninitialized` ([#24785]).

* `IOContext` accepting key-value `Pair`s ([#23271]).

* `pairs` for iterating over key-value `Pair`s ([#22907]).

* `get` do-block syntax supported when using `ENV` ([#23412]).

* `Some{T}` wraps `T` to signify that a result of `T<:Void` is expected ([#23642]).

* `replace` accepts a pair of pattern and replacement, with the number of replacements as
  a keyword argument ([#25165]).

* `CartesianIndices` and `LinearIndices` types represent cartesian and linear indices of
  an array (respectively), and indexing such objects allows translating from one kind of index
  to the other ([#25113]).

## Renaming

* `takebuf_array` is now a method of `take!`. `takebuf_string(io)` becomes `String(take!(io))` ([#19088])

* `is_apple`, `is_bsd`, `is_linux`, `is_unix`, and `is_windows` are now `Sys.isapple`, `Sys.isbsd`,
  `Sys.islinux`, `Sys.isunix`, and `Sys.iswindows`, respectively. These are available in the `Compat.Sys`
  submodule. ([#22182])

* `readstring` is replaced by methods of `read`. ([#22864])

* `read(::IO, ::Type{String})`, `read(::AbstractString, ::Type{String})`,
  and `read(::Cmd, ::Type{String})` are defined for 0.6.

* `Range` is now `AbstractRange` ([#23570]).

* `select`* functions (`select`, `select!`, `selectperm`, `selectperm!`) are renamed to
  `partialsort`* (`partialsort`, `partialsort!`, `partialsortperm`, `partialsortperm!`) ([#23051])

* `ctranspose` and `ctranspose!` are now `adjoint` and `adjoint!` ([#23235])

* Math constants (`π`, `pi`, `e`, `γ`, `eulergamma`, `catalan`, `φ`, `golden`) are moved to the
  `MathConstants` module (available as `Compat.MathConstants`).
  The name exported from `Base` for `e` is changed to `ℯ`. ([#23427])

* `isleaftype` is now `isconcrete` ([#23666])

* `IntSet` is now `BitSet` ([#24282])

* `Complex32`, `Complex64`, and `Complex128` are now `ComplexF16`, `ComplexF32`, and
  `ComplexF64`, respectively ([#24647]).

* `JULIA_HOME` is now `Sys.BINDIR`, available in the `Compat.Sys` submodule. ([#25102])

* `Associative` is now `AbstractDict` ([#25012]).

* `indices` is now `axes` ([#25057]). This function is not exported from Compat to avoid
  conflicts with AxisArrays and other such packages.

* `Void` is now `Nothing` with an alias `Cvoid` for C interop ([#25162]).

* `unshift!` and `shift!` are now `pushfirst!` and `popfirst!` ([#25100]).

* `copy!` and `unsafe_copy!` are now `copyto!` and `unsafe_copyto!` ([#24808]).

* `ipermute!` is now `invpermute!` ([#25168]).

## New macros

* `@vectorize_1arg` and `@vectorize_2arg` are deprecated on Julia 0.6 in favor
  of the broadcast syntax ([#17302]). `Compat.@dep_vectorize_1arg` and
  `Compat.@dep_vectorize_2arg` are provided so that packages can still provide
  the deprecated definitions without causing a depwarn in the package itself
  before all the users are upgraded.

  Packages are expected to use this until all users of the deprecated
  vectorized function have migrated. These macros will be dropped when the
  support for `0.6` is dropped from `Compat`.

* `@nospecialize` has been added ([#22666]).

## Other changes

* The `Expr(:macrocall)` has an extra initial argument `__source__`, which can be tested for with `Compat.macros_have_sourceloc`.

## New types

Currently, no new exported types are introduced by Compat.

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

One of the most frequent problems package developers encounter is finding the right
version of `Compat` to add to their REQUIRE. This is meant to be a guide on how to
specify the right lower bound.

* Find the appropriate fix needed for your package from the `Compat` README. Every
function or feature added to `Compat` is documented in its README, so you are
guaranteed to find it.

* Navigate to the [blame page of the README](https://github.com/JuliaLang/Compat.jl/blame/master/README.md)
by clicking on the README file on GitHub, and then clicking on the `blame` button
which can be found in the top-right corner.

* Now find your fix, and then find the corresponding commit ID of that fix on the
left-hand side. Click on the commit ID. This navigates to a page which recorded
that particular commit.

* On the top pane, you should find the list of the tagged versions of Compat that
includes this fix. Find the minimum version from there.

* Now specify the correct minimum version for Compat in your REQUIRE file by
`Compat <version>`

[#17302]: https://github.com/JuliaLang/julia/issues/17302
[#19088]: https://github.com/JuliaLang/julia/issues/19088
[#20005]: https://github.com/JuliaLang/julia/issues/20005
[#20974]: https://github.com/JuliaLang/julia/issues/20974
[#21197]: https://github.com/JuliaLang/julia/issues/21197
[#21709]: https://github.com/JuliaLang/julia/issues/21709
[#22064]: https://github.com/JuliaLang/julia/issues/22064
[#22182]: https://github.com/JuliaLang/julia/issues/22182
[#22350]: https://github.com/JuliaLang/julia/issues/22350
[#22475]: https://github.com/JuliaLang/julia/issues/22475
[#22512]: https://github.com/JuliaLang/julia/issues/22512
[#22629]: https://github.com/JuliaLang/julia/issues/22629
[#22633]: https://github.com/JuliaLang/julia/issues/22633
[#22646]: https://github.com/JuliaLang/julia/issues/22646
[#22666]: https://github.com/JuliaLang/julia/issues/22666
[#22751]: https://github.com/JuliaLang/julia/issues/22751
[#22761]: https://github.com/JuliaLang/julia/issues/22761
[#22864]: https://github.com/JuliaLang/julia/issues/22864
[#22907]: https://github.com/JuliaLang/julia/issues/22907
[#23051]: https://github.com/JuliaLang/julia/issues/23051
[#23235]: https://github.com/JuliaLang/julia/issues/23235
[#23271]: https://github.com/JuliaLang/julia/issues/23271
[#23412]: https://github.com/JuliaLang/julia/issues/23412
[#23427]: https://github.com/JuliaLang/julia/issues/23427
[#23570]: https://github.com/JuliaLang/julia/issues/23570
[#23642]: https://github.com/JuliaLang/julia/issues/23642
[#23666]: https://github.com/JuliaLang/julia/issues/23666
[#23757]: https://github.com/JuliaLang/julia/issues/23757
[#23812]: https://github.com/JuliaLang/julia/issues/23812
[#23931]: https://github.com/JuliaLang/julia/issues/23931
[#24047]: https://github.com/JuliaLang/julia/issues/24047
[#24282]: https://github.com/JuliaLang/julia/issues/24282
[#24361]: https://github.com/JuliaLang/julia/issues/24361
[#24372]: https://github.com/JuliaLang/julia/issues/24372
[#24459]: https://github.com/JuliaLang/julia/issues/24459
[#24605]: https://github.com/JuliaLang/julia/issues/24605
[#24647]: https://github.com/JuliaLang/julia/issues/24647
[#24648]: https://github.com/JuliaLang/julia/issues/24648
[#24652]: https://github.com/JuliaLang/julia/issues/24652
[#24657]: https://github.com/JuliaLang/julia/issues/24657
[#24714]: https://github.com/JuliaLang/julia/issues/24714
[#24785]: https://github.com/JuliaLang/julia/issues/24785
[#24808]: https://github.com/JuliaLang/julia/issues/24808
[#25012]: https://github.com/JuliaLang/julia/issues/25012
[#25021]: https://github.com/JuliaLang/julia/issues/25021
[#25056]: https://github.com/JuliaLang/julia/issues/25056
[#25057]: https://github.com/JuliaLang/julia/issues/25057
[#25100]: https://github.com/JuliaLang/julia/issues/25100
[#25102]: https://github.com/JuliaLang/julia/issues/25102
[#25113]: https://github.com/JuliaLang/julia/issues/25113
[#25162]: https://github.com/JuliaLang/julia/issues/25162
[#25165]: https://github.com/JuliaLang/julia/issues/25165
[#25168]: https://github.com/JuliaLang/julia/issues/25168
