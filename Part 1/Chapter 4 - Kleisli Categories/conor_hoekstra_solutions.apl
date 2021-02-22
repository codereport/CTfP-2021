nullopt       ← 0 ⍬
has_value     ← ⊃
value         ← ⊃⌽
make_optional ← 1,⊢  

safe_root       ← { ⍵≥0 : make_optional ⍵*.5 ⋄ nullopt }
safe_reciprocal ← { ⍵≠0 : make_optional ÷⍵   ⋄ nullopt }

safe_root_reciprocal ← {
   r ← safe_reciprocal ⍵
   has_value r : safe_root value r ⋄ nullopt
}  
      
safe_root 2            ⍝ 1 1.414213562
safe_reciprocal 2      ⍝ 1 0.5
safe_root_reciprocal 2 ⍝ 1 0.7071067812
