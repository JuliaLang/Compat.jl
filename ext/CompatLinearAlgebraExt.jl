module CompatLinearAlgebraExt
using LinearAlgebra

Base.@deprecate_binding set_num_threads LinearAlgebra.BLAS.set_num_threads false
Base.@deprecate_binding get_num_threads LinearAlgebra.BLAS.get_num_threads false
end
