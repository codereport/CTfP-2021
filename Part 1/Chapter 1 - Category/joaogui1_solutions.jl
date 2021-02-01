### 1
identity
### 2
∘
### 3
function test_compose(f, a)
    @assert (identity ∘ f)(a) == f(a)
    @assert (f ∘ identity)(a) == f(a)
end
test_compose((sqrt), 5)
### 4
#=
Not really, as websites don't always have a link to themselves, nor does A linking to B
to C implies A linking to C
=#
### 5
#=
Friendships are not composable
=#
### 6
#=
Every node must have a self-loop and (A, B) ∈ E and (B, C) ∈ E ⟹ (A, C) ∈ E
=#
