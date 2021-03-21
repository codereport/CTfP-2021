### A Pluto.jl notebook ###
# v0.12.21

using Markdown
using InteractiveUtils

# ╔═╡ 3f70bc24-8a39-11eb-0115-63ce4e9ab843
maybe_to_either(a) = convert(Union{typeof(a), Nothing}, a)

# ╔═╡ 77d9d29c-8a40-11eb-1938-4b798b9c0144
a = maybe_to_either(5)

# ╔═╡ 742e532a-8a40-11eb-3993-8bb91ae30718
function either_to_maybe(a)
	isnothing(a) && return nothing
	a
end

# ╔═╡ 00e79934-8a41-11eb-2ea1-4b989cbe4561
either_to_maybe(nothing)

# ╔═╡ 33e1df98-8a41-11eb-193c-d7734c7179c1
begin
	abstract type Shape end
	function area(s::Shape) end
	function perimeter(s::Shape) end
end

# ╔═╡ 51f5b614-8a42-11eb-3bd3-2718d7168a00
begin
	struct Circle <: Shape
		r::Real
	end
	function area(s::Circle)
		(s.r^2)π
	end
	function perimeter(s::Circle)
		2(π*s.r)
	end
	c = Circle(1)
	area(c), perimeter(c)
end

# ╔═╡ c91136da-8a42-11eb-3687-c92c7b2b5746
begin
	struct Rect <: Shape
		d::Real
		h::Real
	end
	function area(s::Rect)
		s.d*s.h
	end
	function perimeter(s::Rect)
		2(s.d+s.h)
	end
	r = Rect(3, 4)
	area(r), perimeter(r)
end

# ╔═╡ 4efde5ea-8a43-11eb-3342-cdf8001fd1c0
begin
	struct Square <: Shape 
		l::Real
	end
	function area(s::Square)
		s.l^2
	end
	function perimeter(s::Square)
		4s.l
	end
	s = Square(3)
	area(s), perimeter(s)
end

# ╔═╡ Cell order:
# ╠═3f70bc24-8a39-11eb-0115-63ce4e9ab843
# ╠═77d9d29c-8a40-11eb-1938-4b798b9c0144
# ╠═742e532a-8a40-11eb-3993-8bb91ae30718
# ╠═00e79934-8a41-11eb-2ea1-4b989cbe4561
# ╠═33e1df98-8a41-11eb-193c-d7734c7179c1
# ╠═51f5b614-8a42-11eb-3bd3-2718d7168a00
# ╠═c91136da-8a42-11eb-3687-c92c7b2b5746
# ╠═4efde5ea-8a43-11eb-3342-cdf8001fd1c0
