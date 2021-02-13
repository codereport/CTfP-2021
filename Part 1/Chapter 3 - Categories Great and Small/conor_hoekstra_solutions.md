**Question 1**

**Question 2**

a) Partial order<br>
b) Partial order

**Question 3**
```apl
⍝ AND (∧)
1∧1 ⍝ 1
1∧0 ⍝ 0
0∧1 ⍝ 0
0∧0 ⍝ 0
∧/⍬ ⍝ 1 (identity)

⍝ OR (∨)
1∨1 ⍝ 1
1∨0 ⍝ 1
0∨1 ⍝ 1
0∨0 ⍝ 0
∨/⍬ ⍝ 0 (identity)
```

**Question 4**

Single object is the `Bool` Type. There are an infinite number of morphisms. There are two morphisms, `AND False` and `AND True`. When composed, you get:
| A | B | A ∘ B |
|:-:|:-:|:-:|
|`AND True`|`AND True`|`AND True`|
|`AND True`|`AND False`|`AND False`|
|`AND False`|`AND True`|`AND False`|
|`AND False`|`AND False`|`AND False`|

**Question 5**
