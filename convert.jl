import Base: convert, promote_rule

type MyBox
    obj
end

promote_rule{T<:Number}(::Type{T}, ::Type{MyBox}) = T

convert{T<:Number}(::Type{T}, box::MyBox) = box.obj

add(a::Number, b::Number) = a + b


macro enable_promotion(func)
    bare_name = esc(func)
    :( function $bare_name(args...); $bare_name(promote(args...)...); end )
end
