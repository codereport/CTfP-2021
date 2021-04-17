from abc import abstractmethod, ABC
from math import pi

class Maybe(ABC):
    @abstractmethod
    def is_nothing(self):
        pass

    @abstractmethod
    def value(self):
        pass

class Nothing(Maybe):
    def is_nothing(self):
        return True

    def value(self):
        raise ValueError("Cannot get value from 'Nothing'.")

    def __str__(self): return "Nothing"

class Just(Maybe):
    def __init__(self, value):
        self.val = value

    def is_nothing(self):
        return False

    def value(self):
        return self.val

    def __str__(self): return f"Just({self.val})"


class Either:
    """Store a value on the left or on the right of a 2-item tuple."""
    def __init__(self):
        self.values = (None, None)

    def is_left(self):
        return self.values[0] is not None

    def is_right(self):
        return self.values[1] is not None

    def from_left(self):
        if not self.is_left():
            raise ValueError("No 'left' value to get.")
        return self.values[0]

    def from_right(self):
        if not self.is_right():
            raise ValueError("No 'right' value to get.")
        return self.values[1]

class Left(Either):
    def __init__(self, value):
        self.values = (value, None)

    def __str__(self): return f"Left({self.values[0]})"

class Right(Either):
    def __init__(self, value):
        self.values = (None, value)

    def __str__(self): return f"Right({self.values[1]})"

def maybe_to_either_unit(maybe: Maybe) -> Either:
    if maybe.is_nothing():
        return Left(0)      # represent "unit" as 0
    else:
        return Right(maybe.value())

def either_unit_to_maybe(either: Either) -> Maybe:
    if either.is_left():
        return Nothing()
    else:
        return Just(either.from_right())


class Shape(ABC):
    @abstractmethod
    def area(self):
        pass

class Circle(Shape):
    def __init__(self, r):
        self.r = r

    def area(self):
        return pi * self.r**2

class Rect(Shape):
    def __init__(self, w, h):
        self.w, self.h = w, h

    def area(self):
        return self.w * self.h

class Square(Rect):
    def __init__(self, l):
        super().__init__(l, l)

if __name__ == "__main__":
    print(maybe_to_either_unit(Just(42)))
    print(either_unit_to_maybe(Right(1729)))

    print(Circle(3).area())
    print(Rect(10, 5).area())
    print(Square(10).area())
