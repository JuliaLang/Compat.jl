#! /usr/bin/env julia

const COMPAT_REPO = abspath(dirname(@__FILE__), "..")
const COMPAT_README_PATH = joinpath(COMPAT_REPO, "README.md")

function usage()
    str = """
    version.jl - Find the Compat.jl version which introduced a feature

        version.jl <feature> [<occurence>]

    feature - Feature code snippet copied from README.md
    occurence - Which occurence of <feature> should be sought for (optional)
    """
    println(str)
end

function findversion(readme, feature, occurence)
    feature = strip(feature, '`')
    lines = open(readlines, readme, "r")
    idx = findline(lines, feature, occurence)
    if idx == 0
        error("Feature '$feature' could not be found.")
    end
    repo, file = splitdir(readme)
    commit = getcommit(repo, file, idx)
    gettag(repo, commit)
end

findversion(feature, occurence=1) = findversion(COMPAT_README_PATH,
    feature, occurence)

function findline(lines, feature, idx)
    findfirst(cumsum(map(x->countterm(x, feature), lines)) .== idx)
end

function countterm(line, term)
    code = split(line, '`')[2:2:end]
    countnz(map(x->contains(x, term), code))
end

function getcommit(repo, file, idx)
    lines = Vector{String}()
    cd(repo) do
        lines = readlines(`git blame --line-porcelain -L$idx,$idx -- $file`)
    end
    split(lines[1])[1]
end

function gettag(repo, commit)
    lines = Vector{String}()
    cd(repo) do
        lines = readlines(`git tag --contains $commit`)
    end
    strip(lines[1])
end

if (length(ARGS) == 1 && ARGS[1] in ("--help", "-h")) || isempty(ARGS)
    usage()
else
    println(findversion(ARGS...))
end
