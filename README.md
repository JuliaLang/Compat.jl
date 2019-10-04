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

* `@compat foo(::CartesianRange{N})` to replace the former
  `foo(::CartesianRange{CartesianIndex{N}})` ([#20974]). Note that
  `CartesianRange` now has two type parameters, so using them as
  fields in other `struct`s requires manual intervention.

* Required keyword arguments ([#25830]). For example, `@compat foo(; x, y)` makes `x` and `y` required keyword arguments: when calling `foo`, an error is thrown if `x` or `y` is not explicitly provided.

## Module Aliases

* `using Compat.Unicode` is provided on versions older than 0.7, where this library is not
  yet a part of the standard library. ([#25021])

* `using Compat.Distributed` is provided on versions older than 0.7, where this library is
  not yet part of the standard library ([#24443]).

* `using Compat.Pkg` is provided on versions older than 0.7, where this library is
  not yet part of the standard library ([#25705]). Note that `Compat.Pkg` will point to
  the new package manager on 0.7 which does not have a fully compatible API with the old
  package manager.

* `using Compat.InteractiveUtils` is provided on versions older than 0.7, where this library is
  not yet part of the standard library ([#25780]).

* `using Compat.LibGit2` is provided on versions older than 0.7, where this library is
  not yet part of the standard library ([#25706]).

* `using Compat.UUIDs` is provided on versions older than 0.7, where this library is
  not yet part of the standard library ([#25819]).

## New functions, macros, and methods

* `only(x)` returns the one-and-only element of a collection `x`. ([#33129])

* `mod` now accepts a unit range as the second argument ([#32628]).

* `eachrow`, `eachcol`, and `eachslice` to iterate over first, second, or given dimension
  of an array ([#29749]).

* `isnothing` for testing if a variable is equal to `nothing` ([#29674]).

* `@compat finalizer(func, obj)` with the finalizer to run as the first argument and the object to be finalized
  as the second ([#24605]).

* `Some{T}` wraps `T` to signify that a result of `T<:Void` is expected ([#23642]).

* `replace` accepts a pair of pattern and replacement, with the number of replacements as
  a keyword argument ([#25165]).

* `CartesianIndices` and `LinearIndices` types represent cartesian and linear indices of
  an array (respectively), and indexing such objects allows translating from one kind of index
  to the other ([#25113]).

* `codeunits(s)` returns an array-like view of the `UInt8` code units of
  a string and `ncodeunits(s)` returns the number of code units ([#25241]).
  `codeunit(s)` returns the type of the code units of `s` ([#24999]).

* `thisind(s, i)` returns the character index for codeunit `i` ([#24414]).

* Three-argument methods `prevind(s,i,n)`, `nextind(s,i,n)` ([#23805]), and `length(s,i,j)` ([#24999]); the latter two replace `chr2ind` and `ind2chr` in Julia 0.7, respectively.

* `printstyled` prints to a given stream optionally in color and/or bolded ([#25522]).

* `firstindex` to obtain the first index of an iterable ([#25458]).

* `Compat.names` supporting keyword arguments for `all` and `imported` ([#25647]).

* `Compat.IOBuffer` supporting keyword arguments ([#25873]).

* `Compat.range` supporting positional and keyword arguments flavors ([#25896]), ([#28708]).

* `Compat.trunc`, `Compat.floor`, `Compat.ceil`, `Compat.round`, take a keyword argument
  for `base` and `digits`, `Compat.round` also takes `sigdigits` ([#26156], [#26670]).

* `Compat.mv` and `Compat.cp` with `force` keyword argument ([#26069]).

* `Compat.accumulate`, `Compat.accumulate!`, `Compat.all`, `Compat.any`,
  `Compat.cumprod`, `Compat.cumprod!`, `Compat.cumsum`, `Compat.cumsum!`,
  `Compat.findmax`, `Compat.findmin`, `Compat.mapreduce`, `Compat.maximum`,
  `Compat.minimum`, `Compat.prod`, `Compat.reduce`, `Compat.sort`,
  and `Compat.sum`  with `dims` keyword argument ([#25989],[#26369]).

* `Compat.mapreduce` and `Compat.reduce` with `init` keyword argument ([#27711]).

* `selectdim` to obtain a view of an array with a specified index for a specified dimension ([#26009]).

* `Compat.cat` with `dims` as keyword argument ([#27163])

* Single-argument `permutedims(x)` for matrices and vectors ([#24839]).

* `fetch` for `Task`s ([#25940]).

* `Compat.qr` takes `pivot` as a `Val` _instance_ ([#22475]).

* `Compat.rmul!` provides a subset of the functionality of `LinearAlgebra.rmul!` for
  use with Julia 0.6 ([#25701], [#25812]).

* `isbits(t::Type)` is now `isbitstype(t)` ([#26850]).

* `something` to get the first argument different from `nothing`, unwrapping those
  of the `Some` type ([#27258]).

* `mapslices` with `dims` keyword argument ([#27828]).

* `hasproperty` and `hasfield` ([#28850]).
  `hasproperty` is defined only for Julia 0.7 or later.

* `merge` methods with one and `n` `NamedTuple`s ([#29259]).

## Renaming

* `Display` is now `AbstractDisplay` ([#24831]).

* `reprmime(mime, x)` is now `repr(mime, x)` ([#25990]) and `mimewritable` is now `showable` ([#26089]).

* `strwidth` and `charwidth` are now merged into `textwidth` ([#23667]).

* `Associative` is now `AbstractDict` ([#25012]).

* `indices` is now `axes` ([#25057]). This function is not exported from Compat to avoid
  conflicts with AxisArrays and other such packages.

* `Void` is now `Nothing` with an alias `Cvoid` for C interop ([#25162]).

* `Base.IteratorSize` and `Base.IteratorEltype` are available as
  `Compat.IteratorSize` and `Compat.IteratorEltype` ([#25402]).

* `copy!` and `unsafe_copy!` are now `copyto!` and `unsafe_copyto!` ([#24808]).

* `ipermute!` is now `invpermute!` ([#25168]).

* `module_parent`, `Base.function_module`, and `Base.datatype_module` are now methods of
  a new function called `parentmodule` ([#25629]).

* `module_name`, `Base.function_name`, and `Base.datatype_name` are now methods of a
  new function called `nameof` ([#25622]).

* `find` is now `findall` ([#25545]).

* `search` is now `findfirst`/`findnext` and `rsearch` is now `findlast`/`findprev`,
  sometimes combined with `isequal` or `in` ([#24673], [#26436]).

* `Compat.findfirst`, `Compat.findnext`, `Compat.findlast` and `Compat.findprev`,
  return `nothing` when no match is found (rather than `0` or `0:-1`)
  as on Julia 0.7 ([#24673], [#26149]).

* `findin(a, b)` is now `findall(in(b), a)` ([#24673]).

* `indmin` and `indmax` are now `argmin` and `argmax`, respectively ([#25654]).

* `Compat.indexin` accepts any iterable as first argument, returns `nothing` (rather than `0`)
   for entries with no match and gives the index of the first (rather than the last) match
   ([#25662], [#25998]).

* `gc` and `gc_enable` are now `GC.gc` and `GC.enable`, respectively ([#25616]).

* `endof` is now `lastindex` ([#25458]). (Note that `lastindex(A, n)` is not supported.)

* `nb_available` is now `bytesavailable` ([#25634]).

* `method_exists` is now `hasmethod` ([#25615]).

* `object_id` is now `objectid` ([#25615]).

* `LinSpace` is now `LinRange` ([#25896]).

* `isupper`, `islower`, `ucfirst` and `lcfirst` are now `isuppercase`, `islowercase`,
  `uppercasefirst` and `lowercasefirst` ([#26442]).

* `Compat.split` and `Compat.rsplit` accept `keepempty` keyword argument
  if `splitter` is given as second argument ([#26634])

* `isalpha` is now `isletter` ([#27077]).

* `cfunction` is now `@cfunction` ([#26486]).

* `Unicode.isnumeric` is now available as `isnumeric` ([#25479]).

* `vecnorm` and `vecdot` are now `Compat.norm` and `Compat.dot`, respectively, while the
  old `norm(A::AbstractMatrix, p=2)` is now `Compat.opnorm` ([#27401]).  `import Compat: ⋅`
  to get `Compat.dot` as the binary operator `⋅`.

* `atan2` is now a 2-argument method of `atan` ([#27253]).

* `realmin` and `realmax` are now `floatmin` and `floatmax` ([#28302])

* `squeeze` is now `dropdims` ([#28303], [#26660]).

* `repmat` is now `repeat` ([#26039])

## New macros

* The logging macros `@error`, `@warn`, `@info` and `@debug` can be used as
  `Compat.@error`, `Compat.@warn`, `Compat.@info` and `Compat.@debug` on Julia 0.6 ([#24490]).
  Note that the behavior do not mirror the logging macros in Julia 0.7, instead on Julia 0.6:
  - Messages are printed to `STDERR` (like `info` and `warn` on Julia 0.6) and not to a
    dedicated logging stream.
  - The loglevel can not be controlled, but `Compat.@debug` messages can be turned on/off
    by calling `Compat.enable_debug(true/false)`.
  - Extra metadata sent to the macros are ignored.

  As an alternative, see the MicroLogging.jl package for a logging interface similar to the one in Julia 0.7.

## Other changes

* On versions of Julia that do not contain a Base.Threads module, Compat defines a Threads module containing a no-op `@threads` macro.

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

[#20005]: https://github.com/JuliaLang/julia/issues/20005
[#20974]: https://github.com/JuliaLang/julia/issues/20974
[#21197]: https://github.com/JuliaLang/julia/issues/21197
[#21709]: https://github.com/JuliaLang/julia/issues/21709
[#22064]: https://github.com/JuliaLang/julia/issues/22064
[#22182]: https://github.com/JuliaLang/julia/issues/22182
[#22350]: https://github.com/JuliaLang/julia/issues/22350
[#22435]: https://github.com/JuliaLang/julia/issues/22435
[#22475]: https://github.com/JuliaLang/julia/issues/22475
[#22512]: https://github.com/JuliaLang/julia/issues/22512
[#22629]: https://github.com/JuliaLang/julia/issues/22629
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
[#23667]: https://github.com/JuliaLang/julia/issues/23667
[#23757]: https://github.com/JuliaLang/julia/issues/23757
[#23805]: https://github.com/JuliaLang/julia/issues/23805
[#23931]: https://github.com/JuliaLang/julia/issues/23931
[#24047]: https://github.com/JuliaLang/julia/issues/24047
[#24182]: https://github.com/JuliaLang/julia/issues/24182
[#24282]: https://github.com/JuliaLang/julia/issues/24282
[#24361]: https://github.com/JuliaLang/julia/issues/24361
[#24372]: https://github.com/JuliaLang/julia/issues/24372
[#24414]: https://github.com/JuliaLang/julia/issues/24414
[#24443]: https://github.com/JuliaLang/julia/issues/24443
[#24459]: https://github.com/JuliaLang/julia/issues/24459
[#24490]: https://github.com/JuliaLang/julia/issues/24490
[#24605]: https://github.com/JuliaLang/julia/issues/24605
[#24647]: https://github.com/JuliaLang/julia/issues/24647
[#24652]: https://github.com/JuliaLang/julia/issues/24652
[#24657]: https://github.com/JuliaLang/julia/issues/24657
[#24673]: https://github.com/JuliaLang/julia/issues/24673
[#24785]: https://github.com/JuliaLang/julia/issues/24785
[#24808]: https://github.com/JuliaLang/julia/issues/24808
[#24831]: https://github.com/JuliaLang/julia/issues/24831
[#24839]: https://github.com/JuliaLang/julia/issues/24839
[#24874]: https://github.com/JuliaLang/julia/issues/24874
[#24999]: https://github.com/JuliaLang/julia/issues/24999
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
[#25227]: https://github.com/JuliaLang/julia/issues/25227
[#25241]: https://github.com/JuliaLang/julia/issues/25241
[#25249]: https://github.com/JuliaLang/julia/issues/25249
[#25402]: https://github.com/JuliaLang/julia/issues/25402
[#25458]: https://github.com/JuliaLang/julia/issues/25458
[#25459]: https://github.com/JuliaLang/julia/issues/25459
[#25479]: https://github.com/JuliaLang/julia/issues/25479
[#25496]: https://github.com/JuliaLang/julia/issues/25496
[#25522]: https://github.com/JuliaLang/julia/issues/25522
[#25544]: https://github.com/JuliaLang/julia/issues/25544
[#25545]: https://github.com/JuliaLang/julia/issues/25545
[#25571]: https://github.com/JuliaLang/julia/issues/25571
[#25615]: https://github.com/JuliaLang/julia/issues/25615
[#25616]: https://github.com/JuliaLang/julia/issues/25616
[#25622]: https://github.com/JuliaLang/julia/issues/25622
[#25628]: https://github.com/JuliaLang/julia/issues/25628
[#25629]: https://github.com/JuliaLang/julia/issues/25629
[#25634]: https://github.com/JuliaLang/julia/issues/25634
[#25646]: https://github.com/JuliaLang/julia/issues/25646
[#25647]: https://github.com/JuliaLang/julia/issues/25647
[#25654]: https://github.com/JuliaLang/julia/issues/25654
[#25662]: https://github.com/JuliaLang/julia/issues/25662
[#25701]: https://github.com/JuliaLang/julia/issues/25701
[#25705]: https://github.com/JuliaLang/julia/issues/25705
[#25706]: https://github.com/JuliaLang/julia/issues/25706
[#25738]: https://github.com/JuliaLang/julia/issues/25738
[#25780]: https://github.com/JuliaLang/julia/issues/25780
[#25812]: https://github.com/JuliaLang/julia/issues/25812
[#25819]: https://github.com/JuliaLang/julia/issues/25819
[#25830]: https://github.com/JuliaLang/julia/issues/25830
[#25873]: https://github.com/JuliaLang/julia/issues/25873
[#25896]: https://github.com/JuliaLang/julia/issues/25896
[#25935]: https://github.com/JuliaLang/julia/issues/25935
[#25940]: https://github.com/JuliaLang/julia/issues/25940
[#25959]: https://github.com/JuliaLang/julia/issues/25959
[#25989]: https://github.com/JuliaLang/julia/issues/25989
[#25990]: https://github.com/JuliaLang/julia/issues/25990
[#25998]: https://github.com/JuliaLang/julia/issues/25998
[#26009]: https://github.com/JuliaLang/julia/issues/26009
[#26039]: https://github.com/JuliaLang/julia/issues/26039
[#26069]: https://github.com/JuliaLang/julia/issues/26069
[#26089]: https://github.com/JuliaLang/julia/issues/26089
[#26149]: https://github.com/JuliaLang/julia/issues/26149
[#26156]: https://github.com/JuliaLang/julia/issues/26156
[#26283]: https://github.com/JuliaLang/julia/issues/26283
[#26316]: https://github.com/JuliaLang/julia/issues/26316
[#26365]: https://github.com/JuliaLang/julia/issues/26365
[#26369]: https://github.com/JuliaLang/julia/issues/26369
[#26436]: https://github.com/JuliaLang/julia/issues/26436
[#26442]: https://github.com/JuliaLang/julia/issues/26442
[#26486]: https://github.com/JuliaLang/julia/issues/26486
[#26559]: https://github.com/JuliaLang/julia/issues/26559
[#26634]: https://github.com/JuliaLang/julia/issues/26634
[#26660]: https://github.com/JuliaLang/julia/issues/26660
[#26670]: https://github.com/JuliaLang/julia/issues/26670
[#26850]: https://github.com/JuliaLang/julia/issues/26850
[#27077]: https://github.com/JuliaLang/julia/issues/27077
[#27163]: https://github.com/JuliaLang/julia/issues/27163
[#27253]: https://github.com/JuliaLang/julia/issues/27253
[#27258]: https://github.com/JuliaLang/julia/issues/27258
[#27298]: https://github.com/JuliaLang/julia/issues/27298
[#27401]: https://github.com/JuliaLang/julia/issues/27401
[#27711]: https://github.com/JuliaLang/julia/issues/27711
[#27828]: https://github.com/JuliaLang/julia/issues/27828
[#27834]: https://github.com/JuliaLang/julia/issues/27834
[#28295]: https://github.com/JuliaLang/julia/issues/28295
[#28302]: https://github.com/JuliaLang/julia/issues/28302
[#28303]: https://github.com/JuliaLang/julia/issues/28303
[#28708]: https://github.com/JuliaLang/julia/issues/28708
[#28850]: https://github.com/JuliaLang/julia/issues/28850
[#29259]: https://github.com/JuliaLang/julia/issues/29259
[#29674]: https://github.com/JuliaLang/julia/issues/29674
[#29749]: https://github.com/JuliaLang/julia/issues/29749
[#33129]: https://github.com/JuliaLang/julia/issues/33129
[#32628]: https://github.com/JuliaLang/julia/issues/32628
