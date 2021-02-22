## Question 1
function ∘(f::Function, g::Function)
    function fg(x...)
        y = f(x...)
        isnothing(y) ? nothing : g(y)
    end
end

## Question 2
function safe_reciprocal(x)
    x ≠ 0 ? 1/x : nothing
end
@assert safe_reciprocal(2) == 0.5
@assert isnothing(safe_reciprocal(0))

## Question 3
function safe_root(x)
    x ≥ 0 ? sqrt(x) : nothing
end
safe_root_reciprocal = safe_root ∘ safe_reciprocal
@assert safe_root_reciprocal(4) == 0.5
@assert isnothing(safe_root_reciprocal(0))
@assert isnothing(safe_root_reciprocal(-1))