⍝ 1. 
⊢
⍝ 2. 
∘
⍝ 3. 
      test ← {(⍺⍺∘⊢≡⍺⍺)⍵}
      ⌽ test ⍳5
1
      1∘⌽ test ⍳5
1
⍝ 4. Yes, if we assume links are composoble
⍝ 5. No, friendships are not composable (i.e. I am not necessarily friends with all of my friend's friends)
⍝ 6. If every node has an edge pointing to itself (for identity).
