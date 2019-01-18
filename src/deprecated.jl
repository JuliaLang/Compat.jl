Base.@deprecate_binding StringVector Base.StringVector false

# to be deprecated:

# * `range(start, stop)` (without either `length` nor `step` given)
# * Compat.Random.uuid1, uuid4, uuid_version (in favour of Compat.UUIDs.*)
