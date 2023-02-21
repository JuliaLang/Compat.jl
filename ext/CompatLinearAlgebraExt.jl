module CompatLinearAlgebraExt

import Compat
import LinearAlgebra

Base.@deprecate Compat.set_num_threads(nt) LinearAlgebra.BLAS.set_num_threads(nt) false
Base.@deprecate Compat.get_num_threads LinearAlgebra.BLAS.get_num_threads false

end
