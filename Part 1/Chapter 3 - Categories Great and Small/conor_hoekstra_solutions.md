**Question 1**
![image](https://user-images.githubusercontent.com/36027403/107852858-f4accf80-6de0-11eb-9847-1c909f6df37b.png)

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
![image](https://user-images.githubusercontent.com/36027403/107852822-c9c27b80-6de0-11eb-9b69-d34eca6180e8.png)
