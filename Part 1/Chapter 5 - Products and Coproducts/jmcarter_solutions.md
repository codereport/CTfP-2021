# Challenges 5.8

### Q1: Show that the terminal object is unique up to unique isomorphism
    (IdA)    f     (IdB)
      --A<------->B--
      |-^    g    ^-|
      
Assume Category C with 2 terminal objects, A and B.
By definition, there exist a unique morphism from A to B, that we call f.
By definition, there exist a unique morphism from B to A, that we call g.
By definition, there exist a unique morphism from A to A, that we call IdA.
By definition, there exist a unique morphism from B to B, that we call IdB.

By law of composition, g . f must be a morphism from A to A, and there exist only one candidate, IdA.
By law of composition, f . g must be a morphism from B to B, and there exists only one candidate, IdB. 
Therefore, since g.f = IdA and f.g = IdB, A and B with f and g form an isomorphism. Also, since f is the only morphism from A to B and g is the only morphism from B to A, the isomorphism is unique


### Q2: What is Product of two objects in a POSET?

        D
      / | \
     |  C  |
     |/   \|
     A     B

In a Poset, an arrow means "less than or equal". Therefore, if an object C has arrows to object A and object B, it means it is less than or equal than both A and B. To be the Product of A and B, an object C needs to be "better" than any other object D that also has arrows to both A and B, i.e. there must be an arrow from D to C (i.e, D must be less than or equal to C). So, the product of A and B in a Poset, if it exists, is the largest object that is smaller or equal than both A and B. (If equality is well defined, then it is min(A, B))


### Q3: What is the Coproduct of two objects in a POSET?
By reversing the arrows of Question 2, it is clear that the coproduct of 2 objects in a Poset is the smallest Object that is larger than both A and B. (If equality is well defined, then it is max(A, B))


### Q4: Implement Either as a generic type.
See jmcarter_solutions.py for test code
```python
class Either:

    LEFT = 0
    RIGHT = 1

    class WrongTagError(RuntimeError):
        pass

    def __init__(self, tag, value):
        self._tag = tag
        self._type = type(value)
        self._value = value

    @classmethod
    def make_left(cls, value):
        return cls(Either.LEFT, value)

    @classmethod
    def make_right(cls, value):
        return cls(Either.RIGHT, value)

    def left(self):
        if self._tag == Either.LEFT:
            return self._value

        raise Either.WrongTagError()

    def right(self):
        if self._tag == Either.RIGHT:
            return self._value

        raise Either.WrongTagError()

    def tag(self):
        return self._tag

    def type(self):
        return self._type
```

### Q5: Show that Either is a better product than int for (int, bool) with the given injections

```python
def i(n: int):
    return n

def j(b: bool):
    return int(b)
``` 

It is enough to show that there is a function (morphism) that factorizes `i` and `j`. The following function `m` does so:

```python
def m(e: Either):
    if e.tag() == Either.LEFT:
        return i(e.value())
    elif e.tag() == Either.RIGHT:
        return j(e.value())
```

### Q6: Why is int with 2 injections i and j not "better" than Either Int Bool?

It is not possible to find a unique function from int to Either Int Bool. Here are 2 such functions.

```python
def m1(v: int):
    if v < 0:
        return Either.make_right(True)
    else:
        return Either.make_left(v)


def m2(v: int):
    if v > 0:
        return Either.make_right(False)
    else:
        return Either.make_left(abs(v))
```


### Q7: What about the following injections?
```python
def i(n: int):
    return n if n < 0 else n+2

def j(b: bool):
    return int(b)
```
I believe there is some values that are not mapped properly, namely intmax and intmax - 1. But other than that, although it seems good enough, there is no unique morphism from Either to Int, regardless of the injections chosen from int and bool.

