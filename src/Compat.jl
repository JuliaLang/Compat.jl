module Compat

# Import stdlib packages to avoid breaking code that does e.g. `using Compat.Test`
import Base64
import Dates
import DelimitedFiles
import Distributed
import InteractiveUtils
import LibGit2
import Libdl
import LinearAlgebra
import Markdown
import Mmap
import Pkg
import Printf
import REPL
import Random
import Serialization
import SharedArrays
import Sockets
import SparseArrays
import Statistics
import SuiteSparse
import Test
import UUIDs
import Unicode

# Bindings that were available at the top level in Compat
using Base: notnothing
using Dates: AbstractDateTime
using LinearAlgebra: dot, norm, opnorm, qr, rmul!, tr

include("compatmacro.jl")

if VERSION < v"1.1.0-DEV.506"
    function Base.range(start, stop; length=nothing, step=nothing)
        if length === step === nothing
            throw(ArgumentError("at least one of `length` or `step` must be provided"))
        end
        range(start; stop=stop, length=length, step=step)
    end
end

# https://github.com/JuliaLang/julia/pull/30496
if !isdefined(Base, :hasfield)  # 1.2.0-DEV.272
    Base.@pure hasfield(::Type{T}, name::Symbol) where T =
        Base.fieldindex(T, name, false) > 0
    export hasfield
end
if !isdefined(Base, :hasproperty)
    hasproperty(x, s::Symbol) = s in propertynames(x)
    export hasproperty
end

include("deprecated.jl")

end # module Compat
