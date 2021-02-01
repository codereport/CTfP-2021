# 1
from typing import Any

A = Any

def identity(x: A) -> A:
    return x

# 2
from typing import Callable

func = Callable

def composite_func(g: func, f: func) -> func:
    def g_after_f(x):
        return g(f(x))
    return g_after_f

# 3
from typing import Union

Number = Union[int, float]

def square(x: Number) -> Number:
    return x*x

def test_composite_identity() -> None:
    assert square(5) == 25
    assert composite_func(square, identity)(5) == 25
    assert composite_func(identity, square)(5) == 25

test_composite_identity()

# 4
# Category is defined by objects and arrows. Here, we can consider
# the web-pages to be objects and the links on them as: arrows to
# next page or could be a link to itself. Almost all of the pages contains
# links to itself so, yes World Wide Web could be considered a category.


# 5
# No! Friend of my friend is not necessarily my friend.

# 6
# When each node contains an arrow to itself.
