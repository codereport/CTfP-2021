from functools import reduce

##################
# Challenges 1.4 #
##################


#  1.4.1
def identity(x):
    return x


#  1.4.2 ...
#  this solution allow for composition of many functions, not just 2
#  example : f(g(h(x))) = composite_function(f, g, h)(x).
#  the inner compose function would work for only 2 functions composed.
def composite_function(*func):
    def compose(f, g):
        return lambda x: f(g(x))

    return reduce(compose, func, lambda x: x)


#  1.4.3
def test_composition_respects_identity():
    def add2(x):
        return x + 2

    assert identity(1) == 1
    assert add2(1) == 3
    assert composite_function(identity, add2)(1) == 3
    assert composite_function(add2, identity)(1) == 3


#  1.4.4, is the www a category? Are links morphisms
#  I believe it can be viewed as a category
#  Identity is links to same page

#  1.4.5, is Facebook a category, with people as objects and friendships as morphisms?
#  No, because composition is not well defined, a -> b and b -> c does not imply a -> c
#  (friends of friends are not necessarily friends)

#  1.4.6, when is a directed graph a category
#  if composition is well defined, and identity is well defined for each node in the graph
