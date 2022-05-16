Base.@deprecate_binding set_num_threads LinearAlgebra.BLAS.set_num_threads false
Base.@deprecate_binding get_num_threads LinearAlgebra.BLAS.get_num_threads false
Base.@deprecate_binding parseatom Meta.parseatom false
Base.@deprecate_binding parseall Meta.parseall false
import UUIDs
Base.@deprecate_binding uuid5 UUIDs.uuid5 true
