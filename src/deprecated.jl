if !isdefined(Base, :get_extension)
    # LinearAlgebra is a weakdep, but Julia is old enough to ignore that, so we
    # can import and use it here
    import LinearAlgebra
    Base.@deprecate_binding set_num_threads LinearAlgebra.BLAS.set_num_threads false
    Base.@deprecate_binding get_num_threads LinearAlgebra.BLAS.get_num_threads false
else
    # deprecation is done in the CompatLinearAlgebraExt package extension, but
    # the functions need to be declared here
    function set_num_threads end
    function get_num_threads end
end

Base.@deprecate_binding parseatom Meta.parseatom false
Base.@deprecate_binding parseall Meta.parseall false
import UUIDs
Base.@deprecate_binding uuid5 UUIDs.uuid5 true
