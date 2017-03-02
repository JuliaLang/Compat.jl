const COMPAT_REPO = abspath(@__DIR__, "..")
const COMPAT_README_PATH = joinpath(COMPAT_REPO, "README.md")

function findversion(readme, feature, occurence=1)
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

"""
    findversion(feature, occurence)

`findversion` determines the version of `Compat` in which `feature` was
introduced. `feature` is a string that contains
"""
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
