### A Pluto.jl notebook ###
# v0.12.21

using Markdown
using InteractiveUtils

# ╔═╡ d9183da0-9012-11eb-24b6-118bd3225244
md"# Q1
No we can't as it won't preserve identity

fmap id (Just a) = Nothing $\neq$ id (Just a)
"

# ╔═╡ ab55df66-9013-11eb-3c3b-15d132430b64
md"
# Q2
fmap for reader is $\circ$ (composition)

1) Identity
	fmap id f = id $\circ$ f
              = g
		      = id f

2) Composition
	fmap (g . h) f = (g . f) h
               = g . (f . h)
               = g . (fmap f h)
               = fmap g (fmap f h)
               = (fmap g . fmap f) h
"

# ╔═╡ 655c2f5a-9014-11eb-1e8d-6b36d136c9e1
md"
# Q3"

# ╔═╡ b50a91aa-900e-11eb-048b-f15459943ce8
function reader_fmap(f, g)
	g ∘ f
end

# ╔═╡ 6dd01f02-9014-11eb-127f-75dd6d398168


# ╔═╡ Cell order:
# ╠═d9183da0-9012-11eb-24b6-118bd3225244
# ╠═ab55df66-9013-11eb-3c3b-15d132430b64
# ╠═655c2f5a-9014-11eb-1e8d-6b36d136c9e1
# ╠═b50a91aa-900e-11eb-048b-f15459943ce8
# ╠═6dd01f02-9014-11eb-127f-75dd6d398168
