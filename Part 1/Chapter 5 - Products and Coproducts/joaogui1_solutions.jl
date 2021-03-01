### A Pluto.jl notebook ###
# v0.12.21

using Markdown
using InteractiveUtils

# ╔═╡ 9000c320-79bb-11eb-3794-e7cdab0cf668
md"""
# Q1
The first solution would be to simply say that a terminal object is the dual of an initial object, since the latter is unique up to unique isomorphism so is the former.

Another more explicit solution follows: 

Assume there are two terminal objects A and B in a given category. Then we know that there are morphism unique morphisms $id_A: A \rightarrow A$, $id_B: B  \rightarrow B$, $f: A  \rightarrow B$ and $g: B  \rightarrow A$.

But since they are unique and morphism are composable we also have: $f \circ g = id_A \text{ and } g \circ f = id_B$

Since that's the definition of isomorphism then A and B are isomorphic, and since f and g are unique the isomorphism is unique
"""

# ╔═╡ be5b4d3e-79bc-11eb-1fb7-95166a5e3e56
md"""
# Q2
In a poset an arrow means $\leq$. As such the product C of A and B must have the following properties:
+ C $\leq$ A
+ C $\leq$ B
+ For all D such that D $\leq$ A and D $\leq$ B, we have D $\leq$ C
So, the product of A and B in a Poset, if it exists, is the largest object that is smaller or equal than both A and B, also known as the [LCA](https://en.wikipedia.org/wiki/Lowest_common_ancestor) of A and B.
""" 

# ╔═╡ 6a391104-79bd-11eb-0407-cdef236dc1ce
md"""
# Q3
In the opposite category of a poset the arrows mean $\geq$. So in an analogous manner to Q2 we know that the coproduct will be the smallest element which is greater or equal than both A and B
"""

# ╔═╡ d0df53dc-79bd-11eb-33c9-ef5cfba0b289
md"""
# Q4
We can either use Julia's `Union` type or make a tagged Union struct
"""

# ╔═╡ df042bd2-7a25-11eb-0ea9-fdbf58313c92
struct Either{T, U}
    	isleft::Bool
    	val::Union{T, U}
end

# ╔═╡ f8351a4e-7a25-11eb-0c0f-85faceede1b2
md"""
# Q5
We just need to show that there is a morphism that factorizes p and q
"""

# ╔═╡ 1d5c9a10-7a26-11eb-272b-09d60b5d3c6c
begin
	p(n::Int) = n
	q(b::Bool) = int(b)
	function m(e::Either{Int, Bool})
		if e.isleft
			p(e.val)
		else
			q(e.val)
		end
	end
end

# ╔═╡ 39f63c4c-7a26-11eb-3494-7374cf5c1d04
# Using Julia's native Unions
begin
	m(e::Int)  = p(e)
	m(e::Bool) = q(e)
end

# ╔═╡ 0b9cb866-7a27-11eb-0297-211d3a23fa1e
md"""
# Q6
One can define more than one morphism from `Int` to `Either{Int, Bool}`
"""

# ╔═╡ 8bcca1ae-7a27-11eb-21ee-9d81e1448f14
md"""
# Q7 
You will overflow Int
"""

# ╔═╡ Cell order:
# ╠═9000c320-79bb-11eb-3794-e7cdab0cf668
# ╠═be5b4d3e-79bc-11eb-1fb7-95166a5e3e56
# ╠═6a391104-79bd-11eb-0407-cdef236dc1ce
# ╠═d0df53dc-79bd-11eb-33c9-ef5cfba0b289
# ╠═df042bd2-7a25-11eb-0ea9-fdbf58313c92
# ╠═f8351a4e-7a25-11eb-0c0f-85faceede1b2
# ╠═1d5c9a10-7a26-11eb-272b-09d60b5d3c6c
# ╠═39f63c4c-7a26-11eb-3494-7374cf5c1d04
# ╠═0b9cb866-7a27-11eb-0297-211d3a23fa1e
# ╠═8bcca1ae-7a27-11eb-21ee-9d81e1448f14
