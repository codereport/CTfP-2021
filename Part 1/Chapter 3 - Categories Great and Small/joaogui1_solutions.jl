### A Pluto.jl notebook ###
# v0.12.21

using Markdown
using InteractiveUtils

# ╔═╡ 5f6e8512-793e-11eb-2a96-2570bc3bd666
using Luxor

# ╔═╡ f9c88e80-7940-11eb-0ffd-67a481279940
md"""
# Question 1
"""

# ╔═╡ 53b910fe-7950-11eb-1e44-ef9a69cdb0b9
function morphism(center, radius, letter, frac=0.75)
	arrow(Point(center.x, -(1+frac)radius), frac*radius, π/2, 0.99π/2, arrowheadlength=15,   arrowheadangle=pi/12, linewidth=1.25)
	label(letter, :N, Point(center.x, -(1+2frac)radius))
end

# ╔═╡ 83780142-794b-11eb-1a45-e11709af1a71
function obj_id(center=Point(0, 0), radius=80)
	sethue("light blue")
    circle(center, radius, :fill)
	sethue("green")
	morphism(center, radius, "id")
end

# ╔═╡ b1e18bf0-793e-11eb-3f77-43d188528ea1
@png obj_id()

# ╔═╡ fafc9f52-794b-11eb-31f4-ab170ad50246
function obj_comp(center=Point(0, 0), radius=80)
	obj_id(center, radius)
	sethue("red")
	morphism(center, radius, "all the compositions of the original arrow", 0.9)
end

# ╔═╡ 6cce669a-793f-11eb-03ca-a30079f410b4
@png obj_comp()

# ╔═╡ 1b43a4f2-793f-11eb-2d90-33602e12f8db
@png begin
	obj_id(Point(-120, 0), 60)
	obj_id(Point(120, 0), 60)
	sethue("red")
	arrow(Point(-60, 0), Point(60, 0))
end

# ╔═╡ cc49b210-794d-11eb-1523-61e60c18417c
function alphabet(center, radius)
	for (θ, letter) ∈ zip(range(0, (2*25/26)π, length=26), 'A':'Z')
        @layer begin
            rotate(θ)
			translate(0, -95)
            randomhue()
			morphism(center, radius, string(letter), 0.5)
            fillpreserve()

            randomhue()
            strokepath()
        end
    end
end

# ╔═╡ b6273926-793e-11eb-39db-efe4db394eea
@png begin
	center = Point(0, 0)
	radius = 80
	sethue("light blue")
    circle(center, 2.2radius, :fill)
	alphabet(center, radius)
end

# ╔═╡ 10f87d12-7951-11eb-1433-d95509c6f825
md"""
# Q2
a) Partial Order

b) Partial Order
"""

# ╔═╡ 386a5dd4-7951-11eb-18de-6dce0a23cee1
md"""
# Q3
"""

# ╔═╡ 6bf0c3a0-7951-11eb-3b5d-19db041faa48
# AND is the operation, true is identity
begin
	@assert (true && true) == true
	@assert (true && false) == false
	@assert (false && true) == false
	# So true is identity
	@assert (false && false) == false
	# And we now AND is associative
end

# ╔═╡ 876a5604-7952-11eb-1fdf-3d132cc2da0a
# OR is the operation, false is identity
begin
	@assert (true || true) == true
	@assert (true || false) == true
	@assert (false || true) == true
	# So true is identity
	@assert (false || false) == false
	# And we now OR is associative
end

# ╔═╡ d8dc8b06-7952-11eb-1cce-2b93672c6e96
md"""
# Q4
| A | B | A ∘ B |
|:-:|:-:|:-:|
|`AND True`|`AND True`|`AND True`|
|`AND True`|`AND False`|`AND False`|
|`AND False`|`AND True`|`AND False`|
|`AND False`|`AND False`|`AND False`|
"""

# ╔═╡ 1d3f6bd8-7953-11eb-195c-11acb54b78cd
md"""
# Q5
"""

# ╔═╡ 83140cac-7953-11eb-0a2d-ebc0a857f12f
function z3(center, radius)
	labels = ["+0 (id)", "+1", "+2"]
	for (θ, label) ∈ zip(range(0, (4/3)π, length=3), labels)
        @layer begin
            rotate(θ)
            randomhue()
			morphism(center, radius, label, 0.5)
            fillpreserve()

            randomhue()
            strokepath()
        end
    end
end

# ╔═╡ e6a18800-7953-11eb-0758-b1bc488c4d0b
@png begin
	c = Point(0, 0)
	r = 80
	sethue("light blue")
    circle(c, r, :fill)
	z3(center, radius)
end

# ╔═╡ Cell order:
# ╠═5f6e8512-793e-11eb-2a96-2570bc3bd666
# ╟─f9c88e80-7940-11eb-0ffd-67a481279940
# ╟─53b910fe-7950-11eb-1e44-ef9a69cdb0b9
# ╟─83780142-794b-11eb-1a45-e11709af1a71
# ╟─b1e18bf0-793e-11eb-3f77-43d188528ea1
# ╟─fafc9f52-794b-11eb-31f4-ab170ad50246
# ╠═6cce669a-793f-11eb-03ca-a30079f410b4
# ╟─1b43a4f2-793f-11eb-2d90-33602e12f8db
# ╟─cc49b210-794d-11eb-1523-61e60c18417c
# ╟─b6273926-793e-11eb-39db-efe4db394eea
# ╟─10f87d12-7951-11eb-1433-d95509c6f825
# ╠═386a5dd4-7951-11eb-18de-6dce0a23cee1
# ╠═6bf0c3a0-7951-11eb-3b5d-19db041faa48
# ╠═876a5604-7952-11eb-1fdf-3d132cc2da0a
# ╟─d8dc8b06-7952-11eb-1cce-2b93672c6e96
# ╟─1d3f6bd8-7953-11eb-195c-11acb54b78cd
# ╠═83140cac-7953-11eb-0a2d-ebc0a857f12f
# ╠═e6a18800-7953-11eb-0758-b1bc488c4d0b
