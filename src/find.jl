const ByteArray = Union{Array{Int8,1}, Array{UInt8,1}}

## from array.jl

"""
    findnext(A, i::Integer)

Find the next linear index >= `i` of a `true` element of `A`, or `nothing` if not found.

# Examples
```jldoctest
julia> A = [false false; true false]
2×2 Array{Bool,2}:
 false  false
  true  false

julia> findnext(A, 1)
2

julia> findnext(A, 3)
```
"""
function findnext(A, start::Integer)
    l = endof(A)
    i = start
    warned = false
    while i <= l
        a = A[i]
        if !warned && !(a isa Bool)
            depwarn("In the future `findnext` will only work on boolean collections. Use `findnext(x->x!=0, A, start)` instead.", :findnext)
            warned = true
        end
        if a != 0
            return i
        end
        i = nextind(A, i)
    end
    return nothing
end

"""
    findfirst(A)

Return the linear index of the first `true` value in `A`.
Return `nothing` if no such value is found.
To search for other kinds of values, pass a predicate as the first argument.

# Examples
```jldoctest
julia> A = [false false; true false]
2×2 Array{Bool,2}:
 false  false
  true  false

julia> findfirst(A)
2

julia> findfirst(falses(3)) == nothing
true
```
"""
findfirst(A) = findnext(A, 1)

"""
    findnext(predicate::Function, A, i::Integer)

Find the next linear index >= `i` of an element of `A` for which `predicate` returns `true`,
or `nothing` if not found.

# Examples
```jldoctest
julia> A = [1 4; 2 2]
2×2 Array{Int64,2}:
 1  4
 2  2

julia> findnext(isodd, A, 1)
1

julia> findnext(isodd, A, 2) == nothing
true
```
"""
function findnext(testf::Function, A, start::Integer)
    l = endof(A)
    i = start
    while i <= l
        if testf(A[i])
            return i
        end
        i = nextind(A, i)
    end
    return nothing
end

"""
    findfirst(predicate::Function, A)

Return the linear index of the first element of `A` for which `predicate` returns `true`.
Return `nothing` if there is no such element.

# Examples
```jldoctest
julia> A = [1 4; 2 2]
2×2 Array{Int64,2}:
 1  4
 2  2

julia> findfirst(iseven, A)
2

julia> findfirst(x -> x>10, A) == nothing
true

julia> findfirst(equalto(4), A)
3
```
"""
findfirst(testf::Function, A) = findnext(testf, A, 1)

"""
    findprev(A, i::Integer)

Find the previous linear index <= `i` of a `true` element of `A`, or `nothing` if not found.

# Examples
```jldoctest
julia> A = [false false; true true]
2×2 Array{Bool,2}:
 false  false
  true   true

julia> findprev(A,2)
2

julia> findprev(A,1) == nothing
true
```
"""
function findprev(A, start::Integer)
    i = start
    warned = false
    while i >= 1
        a = A[i]
        if !warned && !(a isa Bool)
            depwarn("In the future `findprev` will only work on boolean collections. Use `findprev(x->x!=0, A, start)` instead.", :findprev)
            warned = true
        end
        a != 0 && return i
        i = prevind(A, i)
    end
    return nothing
end

"""
    findlast(A)

Return the linear index of the last `true` value in `A`.
Return `nothing` if there is no `true` value in `A`.

# Examples
```jldoctest
julia> A = [true false; true false]
2×2 Array{Bool,2}:
 true  false
 true  false

julia> findlast(A)
2

julia> A = falses(2,2);

julia> findlast(A) == nothing
true
```
"""
findlast(A) = findprev(A, endof(A))

"""
    findprev(predicate::Function, A, i::Integer)

Find the previous linear index <= `i` of an element of `A` for which `predicate` returns `true`, or
`nothing` if not found.

# Examples
```jldoctest
julia> A = [4 6; 1 2]
2×2 Array{Int64,2}:
 4  6
 1  2

julia> findprev(isodd, A, 1) == nothing
true

julia> findprev(isodd, A, 3)
2
```
"""
function findprev(testf::Function, A, start::Integer)
    i = start
    while i >= 1
        testf(A[i]) && return i
        i = prevind(A, i)
    end
    return nothing
end

"""
    findlast(predicate::Function, A)

Return the linear index of the last element of `A` for which `predicate` returns `true`.
Return `nothing` if there is no such element.

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> findlast(isodd, A)
2

julia> findlast(x -> x > 5, A) == nothing
true
```
"""
findlast(testf::Function, A) = findprev(testf, A, endof(A))



## bitarray.jl

function unsafe_bitfindnext(Bc::Vector{UInt64}, start::Integer)
    chunk_start = Base._div64(start-1)+1
    within_chunk_start = Base._mod64(start-1)
    mask = Base._msk64 << within_chunk_start

    @inbounds begin
        if Bc[chunk_start] & mask != 0
            return (chunk_start-1) << 6 + trailing_zeros(Bc[chunk_start] & mask) + 1
        end

        for i = chunk_start+1:length(Bc)
            if Bc[i] != 0
                return (i-1) << 6 + trailing_zeros(Bc[i]) + 1
            end
        end
    end
    return nothing
end

function findnext(B::BitArray, start::Integer)
    start > 0 || throw(BoundsError(B, start))
    start > length(B) && return nothing
    unsafe_bitfindnext(B.chunks, start)
end

# aux function: same as findnext(~B, start), but performed without temporaries
function findnextnot(B::BitArray, start::Integer)
    start > 0 || throw(BoundsError(B, start))
    start > length(B) && return nothing

    Bc = B.chunks
    l = length(Bc)
    l == 0 && return nothing

    chunk_start = Base._div64(start-1)+1
    within_chunk_start = Base._mod64(start-1)
    mask = ~(Base._msk64 << within_chunk_start)

    @inbounds if chunk_start < l
        if Bc[chunk_start] | mask != Base._msk64
            return (chunk_start-1) << 6 + trailing_ones(Bc[chunk_start] | mask) + 1
        end
        for i = chunk_start+1:l-1
            if Bc[i] != Base._msk64
                return (i-1) << 6 + trailing_ones(Bc[i]) + 1
            end
        end
        if Bc[l] != Base._msk_end(B)
            return (l-1) << 6 + trailing_ones(Bc[l]) + 1
        end
    elseif Bc[l] | mask != Base._msk_end(B)
        return (l-1) << 6 + trailing_ones(Bc[l] | mask) + 1
    end
    return nothing
end
findfirstnot(B::BitArray) = findnextnot(B,1)

function findnext(testf::Function, B::BitArray, start::Integer)
    f0::Bool = testf(false)
    f1::Bool = testf(true)
    !f0 && f1 && return findnext(B, start)
    f0 && !f1 && return findnextnot(B, start)

    start > 0 || throw(BoundsError(B, start))
    start > length(B) && return nothing
    f0 && f1 && return Int(start)
    return nothing # last case: !f0 && !f1
end

function unsafe_bitfindprev(Bc::Vector{UInt64}, start::Integer)
    chunk_start = Base._div64(start-1)+1
    mask = Base._msk_end(start)

    @inbounds begin
        if Bc[chunk_start] & mask != 0
            return (chunk_start-1) << 6 + (64 - leading_zeros(Bc[chunk_start] & mask))
        end

        for i = (chunk_start-1):-1:1
            if Bc[i] != 0
                return (i-1) << 6 + (64 - leading_zeros(Bc[i]))
            end
        end
    end
    return nothing
end

function findprev(B::BitArray, start::Integer)
    start > 0 || return nothing
    start > length(B) && throw(BoundsError(B, start))
    unsafe_bitfindprev(B.chunks, start)
end

function findprevnot(B::BitArray, start::Integer)
    start > 0 || return nothing
    start > length(B) && throw(BoundsError(B, start))

    Bc = B.chunks

    chunk_start = Base._div64(start-1)+1
    mask = ~Base._msk_end(start)

    @inbounds begin
        if Bc[chunk_start] | mask != Base._msk64
            return (chunk_start-1) << 6 + (64 - leading_ones(Bc[chunk_start] | mask))
        end

        for i = chunk_start-1:-1:1
            if Bc[i] != Base._msk64
                return (i-1) << 6 + (64 - leading_ones(Bc[i]))
            end
        end
    end
    return nothing
end
findlastnot(B::BitArray) = findprevnot(B, length(B))

function findprev(testf::Function, B::BitArray, start::Integer)
    f0::Bool = testf(false)
    f1::Bool = testf(true)
    !f0 && f1 && return findprev(B, start)
    f0 && !f1 && return findprevnot(B, start)

    start > 0 || return nothing
    start > length(B) && throw(BoundsError(B, start))
    f0 && f1 && return Int(start)
    return nothing # last case: !f0 && !f1
end

# ## regex.jl
function findnext(re::Regex, str::Union{String,SubString}, idx::Integer)
    if idx > nextind(str,endof(str))
        throw(BoundsError())
    end
    opts = re.match_options
    Base.compile(re)
    Base.PCRE.exec(re.regex, str, idx-1, opts, re.match_data) ?
        ((Int(re.ovec[1])+1):prevind(str,Int(re.ovec[2])+1)) : (0:-1)
end
findnext(r::Regex, s::AbstractString, idx::Integer) = throw(ArgumentError(
    "regex search is only available for the String type; use String(s) to convert"
))
findfirst(r::Regex, s::AbstractString) = findnext(r,s,start(s))

## strings/search.jl
nothing_sentinel(i) = i == 0 ? nothing : i

# function findnext(pred::EqualTo{Char}, s::String, i::Integer)
#     if i < 1 || i > sizeof(s)
#         i == sizeof(s) + 1 && return nothing
#         throw(BoundsError(s, i))
#     end
#     @inbounds isvalid(s, i) || string_index_err(s, i)
#     c = pred.x
#     c ≤ '\x7f' && return nothing_sentinel(_search(s, c % UInt8, i))
#     while true
#         i = _search(s, first_utf8_byte(c), i)
#         i == 0 && return nothing
#         s[i] == c && return i
#         i = next(s, i)[2]
#     end
# end

# findfirst(pred::EqualTo{<:Union{Int8,UInt8}}, a::ByteArray) = nothing_sentinel(_search(a, pred.x))

# findnext(pred::EqualTo{<:Union{Int8,UInt8}}, a::ByteArray, i::Integer) =
#     nothing_sentinel(_search(a, pred.x, i))

function _search(a::Union{String,ByteArray}, b::Union{Int8,UInt8}, i::Integer = 1)
    if i < 1
        throw(BoundsError(a, i))
    end
    n = sizeof(a)
    if i > n
        return i == n+1 ? 0 : throw(BoundsError(a, i))
    end
    p = pointer(a)
    q = ccall(:memchr, Ptr{UInt8}, (Ptr{UInt8}, Int32, Csize_t), p+i-1, b, n-i+1)
    q == C_NULL ? 0 : Int(q-p+1)
end

function _search(a::ByteArray, b::Char, i::Integer = 1)
    if isascii(b)
        _search(a,UInt8(b),i)
    else
        _search(a,unsafe_wrap(Vector{UInt8},string(b)),i).start
    end
end

# function findprev(pred::EqualTo{Char}, s::String, i::Integer)
#     c = pred.x
#     c ≤ '\x7f' && return nothing_sentinel(_rsearch(s, c % UInt8, i))
#     b = first_utf8_byte(c)
#     while true
#         i = _rsearch(s, b, i)
#         i == 0 && return nothing
#         s[i] == c && return i
#         i = prevind(s, i)
#     end
# end

# findlast(pred::EqualTo{<:Union{Int8,UInt8}}, a::ByteArray) = nothing_sentinel(_rsearch(a, pred.x))

# findprev(pred::EqualTo{<:Union{Int8,UInt8}}, a::ByteArray, i::Integer) =
#     nothing_sentinel(_rsearch(a, pred.x, i))

function _rsearch(a::Union{String,ByteArray}, b::Union{Int8,UInt8}, i::Integer = sizeof(a))
    if i < 1
        return i == 0 ? 0 : throw(BoundsError(a, i))
    end
    n = sizeof(a)
    if i > n
        return i == n+1 ? 0 : throw(BoundsError(a, i))
    end
    p = pointer(a)
    q = ccall(:memrchr, Ptr{UInt8}, (Ptr{UInt8}, Int32, Csize_t), p, b, i)
    q == C_NULL ? 0 : Int(q-p+1)
end

function _rsearch(a::ByteArray, b::Char, i::Integer = length(a))
    if isascii(b)
        _rsearch(a,UInt8(b),i)
    else
        _rsearch(a,unsafe_wrap(Vector{UInt8},string(b)),i).start
    end
end

"""
    findfirst(pattern::AbstractString, string::AbstractString)
    findfirst(pattern::Regex, string::String)

Find the first occurrence of `pattern` in `string`. Equivalent to
[`findnext(pattern, string, start(s))`](@ref).

# Examples
```jldoctest
julia> findfirst("z", "Hello to the world")
0:-1

julia> findfirst("Julia", "JuliaLang")
1:5
```
"""
findfirst(pattern::AbstractString, string::AbstractString) =
    findnext(pattern, string, start(string))

# AbstractString implementation of the generic findnext interface
function findnext(testf::Function, s::AbstractString, i::Integer)
    z = ncodeunits(s) + 1
    1 ≤ i ≤ z || throw(BoundsError(s, i))
    @inbounds i == z || isvalid(s, i) || string_index_err(s, i)
    while !done(s,i)
        d, j = next(s,i)
        if testf(d)
            return i
        end
        i = j
    end
    return nothing
end

function _searchindex(s::Union{AbstractString,ByteArray},
                      t::Union{AbstractString,Char,Int8,UInt8},
                      i::Integer)
    if isempty(t)
        return 1 <= i <= nextind(s,endof(s)) ? i :
               throw(BoundsError(s, i))
    end
    t1, j2 = next(t,start(t))
    while true
        i = findnext(equalto(t1),s,i)
        if i === nothing return 0 end
        c, ii = next(s,i)
        j = j2; k = ii
        matched = true
        while !done(t,j)
            if done(s,k)
                matched = false
                break
            end
            c, k = next(s,k)
            d, j = next(t,j)
            if c != d
                matched = false
                break
            end
        end
        if matched
            return i
        end
        i = ii
    end
end

_searchindex(s::AbstractString, t::Char, i::Integer) = coalesce(findnext(equalto(t), s, i), 0)

function _search_bloom_mask(c)
    UInt64(1) << (c & 63)
end

_nthbyte(s::String, i) = codeunit(s, i)
_nthbyte(a::Union{AbstractVector{UInt8},AbstractVector{Int8}}, i) = a[i]

function _searchindex(s::String, t::String, i::Integer)
    # Check for fast case of a single byte
    endof(t) == 1 && return coalesce(findnext(equalto(t[1]), s, i), 0)
    _searchindex(unsafe_wrap(Vector{UInt8},s), unsafe_wrap(Vector{UInt8},t), i)
end

function _searchindex(s::ByteArray, t::ByteArray, i::Integer)
    n = sizeof(t)
    m = sizeof(s)

    if n == 0
        return 1 <= i <= m+1 ? max(1, i) : 0
    elseif m == 0
        return 0
    elseif n == 1
        return coalesce(findnext(equalto(_nthbyte(t,1)), s, i), 0)
    end

    w = m - n
    if w < 0 || i - 1 > w
        return 0
    end

    bloom_mask = UInt64(0)
    skip = n - 1
    tlast = _nthbyte(t,n)
    for j in 1:n
        bloom_mask |= _search_bloom_mask(_nthbyte(t,j))
        if _nthbyte(t,j) == tlast && j < n
            skip = n - j - 1
        end
    end

    i -= 1
    while i <= w
        if _nthbyte(s,i+n) == tlast
            # check candidate
            j = 0
            while j < n - 1
                if _nthbyte(s,i+j+1) != _nthbyte(t,j+1)
                    break
                end
                j += 1
            end

            # match found
            if j == n - 1
                return i+1
            end

            # no match, try to rule out the next character
            if i < w && bloom_mask & _search_bloom_mask(_nthbyte(s,i+n+1)) == 0
                i += n
            else
                i += skip
            end
        elseif i < w
            if bloom_mask & _search_bloom_mask(_nthbyte(s,i+n+1)) == 0
                i += n
            end
        end
        i += 1
    end

    0
end

function _search(s::Union{AbstractString,ByteArray},
                 t::Union{AbstractString,Char,Int8,UInt8},
                 i::Integer)
    idx = _searchindex(s,t,i)
    if isempty(t)
        idx:idx-1
    else
        idx:(idx > 0 ? idx + endof(t) - 1 : -1)
    end
end

"""
    findnext(pattern::AbstractString, string::AbstractString, start::Integer)
    findnext(pattern::Regex, string::String, start::Integer)

Find the next occurrence of `pattern` in `string` starting at position `start`.
`pattern` can be either a string, or a regular expression, in which case `string`
must be of type `String`.

The return value is a range of indexes where the matching sequence is found, such that
`s[findnext(x, s, i)] == x`:

`findnext("substring", string, i)` = `start:end` such that
`string[start:end] == "substring"`, or `0:-1` if unmatched.

# Examples
```jldoctest
julia> findnext("z", "Hello to the world", 1)
0:-1

julia> findnext("o", "Hello to the world", 6)
8:8

julia> findnext("Julia", "JuliaLang", 2)
1:5
```
"""
findnext(t::AbstractString, s::AbstractString, i::Integer) = _search(s, t, i)

"""
    findlast(pattern::AbstractString, string::AbstractString)
    findlast(pattern::Regex, string::String)

Find the last occurrence of `pattern` in `string`. Equivalent to
[`findlast(pattern, string, endof(s))`](@ref).

# Examples
```jldoctest
julia> findlast("o", "Hello to the world")
15:15

julia> findfirst("Julia", "JuliaLang")
1:5
```
"""
findlast(pattern::AbstractString, string::AbstractString) =
    findprev(pattern, string, endof(string))

# AbstractString implementation of the generic findprev interface
function findprev(testf::Function, s::AbstractString, i::Integer)
    if i < 1
        return i == 0 ? nothing : throw(BoundsError(s, i))
    end
    n = ncodeunits(s)
    if i > n
        return i == n+1 ? nothing : throw(BoundsError(s, i))
    end
    # r[reverseind(r,i)] == reverse(r)[i] == s[i]
    # s[reverseind(s,j)] == reverse(s)[j] == r[j]
    r = reverse(s)
    j = findnext(testf, r, reverseind(r, i))
    j === nothing ? nothing : reverseind(s, j)
end

function _rsearchindex(s::AbstractString,
                       t::Union{AbstractString,Char,Int8,UInt8},
                       i::Integer)
    if isempty(t)
        return 1 <= i <= nextind(s, endof(s)) ? i :
               throw(BoundsError(s, i))
    end
    t = t isa AbstractString ? reverse(t) : t
    rs = reverse(s)
    l = endof(s)
    t1, j2 = next(t, start(t))
    while true
        i = findprev(equalto(t1), s, i)
        i === nothing && return 0
        c, ii = next(rs, reverseind(rs, i))
        j = j2; k = ii
        matched = true
        while !done(t, j)
            if done(rs, k)
                matched = false
                break
            end
            c, k = next(rs, k)
            d, j = next(t, j)
            if c != d
                matched = false
                break
            end
        end
        matched && return nextind(s, reverseind(s, k))
        i = reverseind(s, ii)
    end
end

function _rsearchindex(s::String, t::String, i::Integer)
    # Check for fast case of a single byte
    if endof(t) == 1
        return coalesce(findprev(equalto(t[1]), s, i), 0)
    elseif endof(t) != 0
        j = i ≤ ncodeunits(s) ? nextind(s, i)-1 : i
        return _rsearchindex(unsafe_wrap(Vector{UInt8}, s), unsafe_wrap(Vector{UInt8}, t), j)
    elseif i > sizeof(s)
        return 0
    elseif i == 0
        return 1
    else
        return i
    end
end

function _rsearchindex(s::ByteArray, t::ByteArray, k::Integer)
    n = sizeof(t)
    m = sizeof(s)

    if n == 0
        return 0 <= k <= m ? max(k, 1) : 0
    elseif m == 0
        return 0
    elseif n == 1
        return coalesce(findprev(equalto(_nthbyte(t,1)), s, k), 0)
    end

    w = m - n
    if w < 0 || k <= 0
        return 0
    end

    bloom_mask = UInt64(0)
    skip = n - 1
    tfirst = _nthbyte(t,1)
    for j in n:-1:1
        bloom_mask |= _search_bloom_mask(_nthbyte(t,j))
        if _nthbyte(t,j) == tfirst && j > 1
            skip = j - 2
        end
    end

    i = min(k - n + 1, w + 1)
    while i > 0
        if _nthbyte(s,i) == tfirst
            # check candidate
            j = 1
            while j < n
                if _nthbyte(s,i+j) != _nthbyte(t,j+1)
                    break
                end
                j += 1
            end

            # match found
            if j == n
                return i
            end

            # no match, try to rule out the next character
            if i > 1 && bloom_mask & _search_bloom_mask(_nthbyte(s,i-1)) == 0
                i -= n
            else
                i -= skip
            end
        elseif i > 1
            if bloom_mask & _search_bloom_mask(_nthbyte(s,i-1)) == 0
                i -= n
            end
        end
        i -= 1
    end

    0
end

function _rsearch(s::Union{AbstractString,ByteArray},
                  t::Union{AbstractString,Char,Int8,UInt8},
                  i::Integer)
    idx = _rsearchindex(s,t,i)
    if isempty(t)
        idx:idx-1
    else
        idx:(idx > 0 ? idx + endof(t) - 1 : -1)
    end
end

"""
    findprev(pattern::AbstractString, string::AbstractString, start::Integer)
    findprev(pattern::Regex, string::String, start::Integer)

Find the previous occurrence of `pattern` in `string` starting at position `start`.
`pattern` can be either a string, or a regular expression, in which case `string`
must be of type `String`.

The return value is a range of indexes where the matching sequence is found, such that
`s[findprev(x, s, i)] == x`:

`findprev("substring", string, i)` = `start:end` such that
`string[start:end] == "substring"`, or `0:-1` if unmatched.

# Examples
```jldoctest
julia> findprev("z", "Hello to the world", 18)
0:-1

julia> findprev("o", "Hello to the world", 18)
15:15

julia> findprev("Julia", "JuliaLang", 6)
1:5
```
"""
findprev(t::AbstractString, s::AbstractString, i::Integer) = _rsearch(s, t, i)

## SparseArrays

_sparse_findnextnz(v::AbstractSparseArray, i::Integer) = (I = find(!iszero, v); n = searchsortedfirst(I, i); n<=length(I) ? I[n] : nothing)
_sparse_findprevnz(v::AbstractSparseArray, i::Integer) = (I = find(!iszero, v); n = searchsortedlast(I, i);  !iszero(n)   ? I[n] : nothing)

function findnext(f::typeof(!iszero), v::AbstractSparseArray, i::Integer)
    j = _sparse_findnextnz(v, i)
    while j !== nothing && !f(v[j])
        j = _sparse_findnextnz(v, j+1)
    end
    return j
end

function findprev(f::typeof(!iszero), v::AbstractSparseArray, i::Integer)
    j = _sparse_findprevnz(v, i)
    while j !== nothing && !f(v[j])
        j = _sparse_findprevnz(v, j-1)
    end
    return j
end

function _sparse_findnextnz(v::SparseVector, i::Integer)
    n = searchsortedfirst(v.nzind, i)
    if n > length(v.nzind)
        return nothing
    else
        return v.nzind[n]
    end
end

function _sparse_findprevnz(v::SparseVector, i::Integer)
    n = searchsortedlast(v.nzind, i)
    if iszero(n)
        return nothing
    else
        return v.nzind[n]
    end
end

function _sparse_findnextnz(m::SparseMatrixCSC, i::Integer)
    if i > length(m)
        return nothing
    end
    row, col = Tuple(CartesianIndices(m)[i])
    lo, hi = m.colptr[col], m.colptr[col+1]
    n = searchsortedfirst(m.rowval, row, lo, hi-1, Base.Order.Forward)
    if lo <= n <= hi-1
        return LinearIndices(m)[m.rowval[n], col]
    end
    nextcol = findnext(c->(c>hi), m.colptr, col+1)
    nextcol === nothing && return nothing
    nextlo = m.colptr[nextcol-1]
    return LinearIndices(m)[m.rowval[nextlo], nextcol-1]
end

function _sparse_findprevnz(m::SparseMatrixCSC, i::Integer)
    if iszero(i)
        return nothing
    end
    row, col = Tuple(CartesianIndices(m)[i])
    lo, hi = m.colptr[col], m.colptr[col+1]
    n = searchsortedlast(m.rowval, row, lo, hi-1, Base.Order.Forward)
    if lo <= n <= hi-1
        return LinearIndices(m)[m.rowval[n], col]
    end
    prevcol = findprev(c->(c<lo), m.colptr, col-1)
    prevcol === nothing && return nothing
    prevhi = m.colptr[prevcol+1]
    return LinearIndices(m)[m.rowval[prevhi-1], prevcol]
end
