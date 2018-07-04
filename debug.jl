import Pkg
@show LOAD_PATH
@show pwd()
@show readdir(pwd())
ctx = Pkg.Types.Context()
@show ctx.env.project_file

project_file = ctx.env.project_file
project = Pkg.Types.read_project(project_file)
@show project
@show k = any(haskey.((project,), ["name", "uuid", "version"]))
project_package = nothing
if k
    project_package = Pkg.Types.PackageSpec(
        get(project, "name", ""),
        UUID(get(project, "uuid", 0)),
        VersionNumber(get(project, "version", "0.0")),
    )
end

@show project_package


@show ctx.env.pkg.name
@show ctx.env.pkg.uuid

pkg = Pkg.Types.PackageSpec("Compat")
@show Pkg.Types.is_project_name(ctx.env, pkg.name)
pkgs = [pkg]
Pkg.Types.project_resolve!(ctx.env, pkgs)
@show pkg.uuid
@show Pkg.Types.ensure_resolved(ctx.env, pkgs)
Pkg.build("Compat")