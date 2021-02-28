⍝ Question 4

left      ← 1,⊢
right     ← 2,⊢
isLeft    ← 1=⊃
isRight   ← 2=⊃
fromLeft  ← { isLeft  ⍵: ⊃⌽⍵ ⋄ 'Fail' ⎕SIGNAL 999 }
fromRight ← { isRight ⍵: ⊃⌽⍵ ⋄ 'Fail' ⎕SIGNAL 999 }
      
l ← left  42
r ← right 1729

isLeft    l ⍝ 1
isRight   r ⍝ 1
fromLeft  l ⍝ 42
fromRight r ⍝ 1729
fromLeft  r ⍝ Fail
fromRight l ⍝ Fail

⍝ Question 5

i ← ⊢
j ← 0∘≠

m ← { isLeft ⍵: fromLeft ⍵ ⋄ 0≠fromRight ⍵ }
