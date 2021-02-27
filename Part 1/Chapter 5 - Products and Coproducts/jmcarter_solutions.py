import pytest


##################
# Challenges 5.8 #
##################

#  5.8.4: Implement Either
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


def test_either():
    either = Either.make_left(42)
    assert either.left() == 42
    assert either.tag() == Either.LEFT
    assert either.type() == int
    with pytest.raises(Either.WrongTagError):
        either.right()

    either = Either.make_right(True)
    assert either.right() is True
    assert either.tag() == Either.RIGHT
    assert either.type() == bool
    with pytest.raises(Either.WrongTagError):
        either.left()


#  5.8.5: Show Either is a "better" coproduct than int for int and bool with following injections
def i(n: int):
    return n


def j(b: bool):
    return int(b)


#  This function "factorizes" both i and j
def m(e: Either):
    if e.tag() == Either.LEFT:
        return i(e.value())
    elif e.tag() == Either.RIGHT:
        return j(e.value())


#  5.8.6: Why is int with 2 injections i and j not "better" than Either Int Bool?
#  We cannot find a unique function from int to Either Int Bool. Here are 2 such functions.

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


#  5.8.7
#  I think there is a problem with intmax and intmax-1 that cannot be represented.