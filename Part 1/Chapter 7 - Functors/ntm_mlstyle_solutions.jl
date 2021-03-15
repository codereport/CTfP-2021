### A Pluto.jl notebook ###
# v0.12.21

using Markdown
using InteractiveUtils

# This Pluto notebook uses @bind for interactivity. When running this notebook outside of Pluto, the following 'mock version' of @bind gives bound variables a default value (instead of an error).
macro bind(def, element)
    quote
        local el = $(esc(element))
        global $(esc(def)) = Core.applicable(Base.get, el) ? Base.get(el) : missing
        el
    end
end

# ╔═╡ 907a8e10-8557-11eb-0fab-73a08d51936e
using MLStyle

# ╔═╡ a2c80d22-8557-11eb-0607-991db6b72cdc
md"We use a macro from MLStyle to create a datatype and its hierarchy. Julia already has *Some{T}* so we are going with another name.

Because Julia uses subtyping instead of typeclasses, and our Maybee datatype uses subtyping to organize things, `Somee{T} <: Maybee{T}` is not supported here."

# ╔═╡ ac6181a6-8557-11eb-3a6f-a91c081f23cd
@data Maybee begin
    Nothingg
    Somee(Any)
end

# ╔═╡ b4692aa2-8557-11eb-095c-13042255353c
md"The *@as_record* macro enables you to destructure arbitrary datatypes with the *@match* macro"

# ╔═╡ bdd46124-8557-11eb-368a-1fb5bba72248
@as_record Some

# ╔═╡ c2415cba-8557-11eb-0968-e14fe3bf763a
md"We show that *Somee* and *Some* are isomorphic by constructing conversions"

# ╔═╡ cdcdbba2-8557-11eb-1c80-af98e533b8d7
maybee2maybe(m::Maybee) = @match m begin
    Nothingg => Nothing
    Somee(x) => Some(x)
end

# ╔═╡ d5a2f476-8557-11eb-28e2-d158f7c907fb
maybe2maybee(m::Union{Some, Nothing}) = @match m begin
    nothing => Nothingg
    Some(x) => Somee(x)
end

# ╔═╡ e30ed924-8557-11eb-23f9-f9e1beb69344
md"The conventional way to destructure datatypes in Julia is through dispatching by type, but this does not offer that kind of destructuring, so you have to destructure it explicitly."

# ╔═╡ eb1b751e-8557-11eb-2d0c-59b03cf3cbd5
begin
	maybee2maybeConventional(m::Somee) = Some(m._1)
	maybee2maybeConventional(Nothingg) = Nothing
	
	maybe2maybeeConventional(m::Some) = Somee(m.value)
	maybe2maybeeConventional(m::Nothing) = Nothingg
end

# ╔═╡ f8c15224-8557-11eb-1972-2d0dbecceec5
md"## fmap"

# ╔═╡ 086618f4-8558-11eb-0cef-2111540d1b17
fmap(f, m::Union{Some, Nothing}) = @match m begin
    nothing => nothing
    Some(x) => Some(f(x))
end

# ╔═╡ 0f75151e-8558-11eb-3628-99dad989426a
fmap(n->factorial(n),Some(5))

# ╔═╡ 0386f9b0-8559-11eb-3e3f-2f589cd27c8f
@bind inputStr html"<input type='text'>"

# ╔═╡ 3eda6538-8559-11eb-2ad0-5d58626036be
someStr = @match inputStr begin
	if occursin("lucifer",lowercase(inputStr)) end => nothing
	x => Some(x)
end

# ╔═╡ 0908cee4-855f-11eb-2c86-dfa45aed43f8
md"Some operators are given infix notation in Julia, we can use this to make fmap infix"

# ╔═╡ 01a96aca-855d-11eb-218c-f9211abe3408
md"## Reader"

# ╔═╡ 0a8d2d7a-855d-11eb-12b4-b7ad60004fa4
⊙(a,b) = fmap(a,b)

# ╔═╡ 687397c4-855a-11eb-0abb-03080c6a812f
md"Maybe we want to count the permutation of strings"

# ╔═╡ 7bf4ab36-855e-11eb-202f-f96c8c2bea8a
factorial ⊙ (length ⊙ someStr)

# ╔═╡ Cell order:
# ╠═907a8e10-8557-11eb-0fab-73a08d51936e
# ╟─a2c80d22-8557-11eb-0607-991db6b72cdc
# ╠═ac6181a6-8557-11eb-3a6f-a91c081f23cd
# ╟─b4692aa2-8557-11eb-095c-13042255353c
# ╠═bdd46124-8557-11eb-368a-1fb5bba72248
# ╟─c2415cba-8557-11eb-0968-e14fe3bf763a
# ╠═cdcdbba2-8557-11eb-1c80-af98e533b8d7
# ╠═d5a2f476-8557-11eb-28e2-d158f7c907fb
# ╟─e30ed924-8557-11eb-23f9-f9e1beb69344
# ╠═eb1b751e-8557-11eb-2d0c-59b03cf3cbd5
# ╟─f8c15224-8557-11eb-1972-2d0dbecceec5
# ╠═086618f4-8558-11eb-0cef-2111540d1b17
# ╠═0f75151e-8558-11eb-3628-99dad989426a
# ╠═0386f9b0-8559-11eb-3e3f-2f589cd27c8f
# ╠═3eda6538-8559-11eb-2ad0-5d58626036be
# ╟─0908cee4-855f-11eb-2c86-dfa45aed43f8
# ╟─01a96aca-855d-11eb-218c-f9211abe3408
# ╠═0a8d2d7a-855d-11eb-12b4-b7ad60004fa4
# ╟─687397c4-855a-11eb-0abb-03080c6a812f
# ╠═7bf4ab36-855e-11eb-202f-f96c8c2bea8a
