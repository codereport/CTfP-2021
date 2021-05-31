### Question 1

```hs
(phi . psi) fa = phi (\h -> fmap h fa)
               = (\h -> fmap h fa) id
               = fmap id fa
               = fa
               
(psi . phi) alpha = psi (phi alpha)
                  = psi (aplha id)
                  = \h -> fmap h (alpha id)
                  = \h -> alpha h id
                  = \h -> alpha h
                  = alpha
```

### Question 2 & 3

See http://danshiebler.com/2018-11-10-category-solutions/
