from typing import TypeVar, Generic
from math import sqrt
from pytest import approx
import random

##################
# Challenges 4.4 #
##################

#  4.4.1 (define category for partial function)
#  f x returns an Optional(a)
#  Composition: g . f x returns an Optional(b), if f returned an empty, g will return an empty
#  Identity : id x = optional(x)


T = TypeVar("T")


class Optional(Generic[T]):
    def __init__(self, value: T = None):
        if value is None:
            self._valid = False
        else:
            self._valid = True

        self._value = value

    def is_valid(self):
        return self._valid

    def get_value(self):
        return self._value if self._valid else None

    def __eq__(self, other: T):
        return self._value == other.get_value() and self._valid == other.is_valid()


def safe_root(x):
    if x >= 0:
        return Optional(sqrt(x))
    else:
        return Optional()


#  4.4.2
def safe_inverse(x):
    if x != 0:
        return Optional(1 / x)
    else:
        return Optional()


#  4.4.3
def compose_safe_functions(f1, f2):
    def composed(x):
        o1 = f1(x)
        if o1.is_valid():
            return f2(o1.get_value())
        else:
            return Optional()

    return composed


#  to compare with composed function
def safe_root_inverse(x):
    o1 = safe_inverse(x)
    if o1.is_valid():
        return safe_root(o1.get_value())
    else:
        return o1


def test_safe_functions():
    assert safe_root(-1).is_valid() is False
    assert safe_root(100).is_valid() is True
    assert safe_root(100).get_value() == 10

    assert safe_inverse(0).is_valid() is False
    assert safe_inverse(100).is_valid() is True
    assert safe_inverse(100).get_value() == approx(0.01)

    assert safe_root_inverse(-1).is_valid() is False
    assert safe_root_inverse(0).is_valid() is False
    assert safe_root_inverse(100).is_valid() is True
    assert safe_root_inverse(100).get_value() == approx(0.1)

    csf = compose_safe_functions
    composed_root_inverse = csf(safe_inverse, safe_root)
    assert composed_root_inverse(-1).is_valid() is False
    assert composed_root_inverse(0).is_valid() is False
    assert composed_root_inverse(100).is_valid() is True
    assert composed_root_inverse(100).get_value() == approx(0.1)

    for _ in range(10):
        val = random.randrange(-10, 10)
        print(val)
        assert composed_root_inverse(val) == safe_root_inverse(val)
