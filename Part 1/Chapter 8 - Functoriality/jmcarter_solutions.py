# Solution Challenge 8.9.5
from typing import Callable


class Bifunctor:
    def bimap(self, f: Callable, g: Callable):
        raise NotImplementedError


def first(f: Callable, bf: Bifunctor):
    return bf.bimap(f, lambda x: x)


def second(g: Callable, bf: Bifunctor):
    return bf.bimap(lambda x: x, g)


def bimap(f: Callable, g: Callable, bf: Bifunctor):
    return bf.bimap(f, g)


class Pair(Bifunctor):
    def __init__(self, a, b):
        self.a = a
        self.b = b

    def bimap(self, f: Callable, g: Callable):
        return Pair(f(self.a), g(self.b))

    def __repr__(self):
        return f'Pair({self.a}, {self.b})'

    def __str__(self):
        return f'({self.a}, {self.b})'

    def __eq__(self, other):
        return self.a == other.a and self.b == other.b


def test_pair_bifunctor():
    assert Pair(5, True).bimap(lambda x: x*x, lambda x: not x) == Pair(25, False)

    assert first(lambda x: x*x, Pair(5, True)) == Pair(25, True)
    assert second(lambda x: not x, Pair(5, True)) == Pair(5, False)
    assert bimap(lambda x: str(x), lambda x: str(x), Pair(5, True)) == Pair("5", "True")
