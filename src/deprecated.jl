# deprecation is done in the CompatLinearAlgebraExt package extension, but
# the functions need to be declared here
function set_num_threads end
function get_num_threads end

Base.@deprecate_binding parseatom Meta.parseatom false
Base.@deprecate_binding parseall Meta.parseall false
import UUIDs
Base.@deprecate_binding uuid5 UUIDs.uuid5 true
