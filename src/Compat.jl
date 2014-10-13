module Compat

if VERSION < v"0.4.0-dev+980"
    macro Dict(pairs...)
        esc(Expr(:dict, pairs...))
    end
    macro AnyDict(pairs...)
        esc(Expr(:typed_dict, :(Any=>Any), pairs...))
    end
    macro TypedDict(pairs...)
        esc(Expr(:typed_dict, pairs...))
    end
else
    macro Dict(pairs...)
        esc(Expr(:call, :Dict, pairs...))
    end
    macro AnyDict(pairs...)
        esc(Expr(:call, :(Base.AnyDict), pairs...))
    end
    macro TypedDict(pairs...)
        esc(Expr(:call, Expr( :curly, :Dict, pairs[1].args[1], pairs[1].args[2] ), pairs[2:end]...))
    end
end

end # module
