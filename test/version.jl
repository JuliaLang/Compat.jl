@test isfile(Compat.COMPAT_README_PATH)
line = "* `@compat Base.IndexStyle(::Type{<:MyArray}) = IndexLinear()` and " *
    "`@compat Base.IndexStyle(::Type{<:MyArray}) = IndexCartesian()` to " *
    "define traits for abstract arrays, replacing the former " *
    "`Base.linearindexing{T<:MyArray}(::Type{T}) = Base.LinearFast()` and " *
    "`Base.linearindexing{T<:MyArray}(::Type{T}) = Base.LinearSlow()`, " *
    "respectively."

@test Compat.countterm(line, "@compat") == 2
@test Compat.countterm(line, "Base.LinearFast()") == 1
@test Compat.countterm(line, "traits") == 0
let lines = open(readlines, Compat.COMPAT_README_PATH)
    @test Compat.findline(lines, "@compat abstract type T end", 1) == 74
    @test Compat.findline(lines, "@compat", 21) == 187
end
repo, file = splitdir(Compat.COMPAT_README_PATH)
@test Compat.getcommit(repo, file, 74) == "c83a1b56d69ff268019dffbd310696115234f521"

@test Compat.findversion("@compat abstract type T end") == "v0.17.0"
@test Compat.findversion("`@compat abstract type T end`") == "v0.17.0"
@test Compat.findversion("@static") == "v0.12.0"
