function depwarn_ex(msg, name)
    return quote
        Base.depwarn($msg, Symbol($name))
    end
end

primarytype(@nospecialize(t)) = t.name.wrapper

export @functorize
macro functorize(f)
    code = f === :scalarmax          ? :(Base.scalarmax) :
           f === :scalarmin          ? :(Base.scalarmin) :
           f === :centralizedabs2fun ? :(primarytype(typeof(Base.centralizedabs2fun(0)))) :
           f
    warning = depwarn_ex("@functorize is deprecated as functor objects are no longer supported in julia", "@functorize")
    return quote
        $warning
        $code
    end
end

Base.@deprecate_binding KERNEL Sys.KERNEL
Base.@deprecate_binding UTF8String Core.String
Base.@deprecate_binding ASCIIString Core.String
Base.@deprecate_binding unsafe_convert Base.unsafe_convert
Base.@deprecate_binding remote_do Distributed.remote_do
Base.@deprecate_binding Filesystem Base.Filesystem
Base.@deprecate_binding AsyncCondition Base.AsyncCondition
Base.@deprecate_binding promote_eltype_op Base.promote_eltype_op
@eval Base.@deprecate_binding $(Symbol("@irrational")) Base.$(Symbol("@irrational"))
@eval Base.@deprecate_binding $(Symbol("@blasfunc")) Compat.LinearAlgebra.BLAS.$(Symbol("@blasfunc"))

# to be deprecated:

# * `range(start, stop)` (without either `length` nor `step` given)
