"""
CATEGORY THEORY FOR PROGRAMMERS
By Bartosz Milewski
Compiled and edited by Igal Tabachnik

Chapter 4
Section 4.4 Challenges

    1. Construct the Kleisli category for partial functions (define composition
        and identity).

        Composition is defined in the Partial class, via the __ge__ method
        (this is the magic method (override) for greater-than-or-equal (>=),
        which is the closest-looking operator to the >=> ("fish") operator in
        Haskell, which is Kleisli composition.

        Identity is handled just below the partial class, as a special value,
        partial_identity, the partial function that always succeeds. Both left
        and right identity are also tested in their success and failure cases
        in test_left_identity_failure, test_left_identity_success,
        test_right_identity_failure, and test_right_identity_success.

    2. Implement the embellished function safe_reciprocal that re-turns a valid
        reciprocal of its argument, if it's different from zero.

        These are implemented just below partial_identity, which is just below
        the Partial class.

    3. Compose the functions safe_root and safe_reciprocal to implement
        safe_root_reciprocal that calculates sqrt(1/x) whenever possible.

        These are composed (and working) in the two tests:
        test_composition_goodInput, and test_composition_badInput.
"""

import unittest
from math import sqrt


class Failure (object):

    def __repr__ (self):
        return self.__class__.__name__ + "()"

    def __eq__ (self, other):
        if isinstance(other, Failure):
            return True
        return False


class Success (object):

    def __init__ (self, value):
        self.value = value

    def __repr__ (self):
        return self.__class__.__name__ + "(" + repr(self.value) + ")"

    def __eq__ (self, other):
        if isinstance(self, Success):
            if isinstance(other, Success):
                return self.value == other.value
        return False


class Partial (object):

    def __init__ (self, f):
        self.f = f

    def raiseOnBadReturnType (self, r):
        if isinstance(r, (Failure, Success)):
            return
        typeErr = "wrong return type: (%s) - should be Success() or Failure()"
        typ = str(type(r).__name__)
        raise TypeError, typeErr % typ

    def __call__ (self, x):
        r = self.f(x)
        self.raiseOnBadReturnType(r)
        return r

    def __ge__ (self, other):
        """
        Kleisli composition for partial functions, via >= operator
        (>= looks similar to the >=> ("fish") operator in Haskell)
        """
        def g (x):
            r1 = self.f(x)
            if isinstance(r1, Failure):
                return r1
            self.raiseOnBadReturnType(r1)
            r2 = other.f(r1.value)
            self.raiseOnBadReturnType(r2)
            return r2
        return Partial(g)


# the partial function that always succeeds
partial_identity = Partial(Success)


safe_root       = Partial(lambda n: Success(sqrt(n)) if n >= 0 else Failure())
safe_reciprocal = Partial(lambda n: Success(1.0/n)   if n != 0 else Failure())


class Test_Partial (unittest.TestCase):

    def test_safe_root_failure (self):
        result = safe_root(-1)
        expected = Failure()
        self.assertEquals(result, expected)

    def test_safe_root_success (self):
        result = safe_root(9)
        expected = Success(3.0)
        self.assertEquals(result, expected)

    def test_safe_reciprocal_failure (self):
        result = safe_reciprocal(0)
        expected = Failure()
        self.assertEquals(result, expected)

    def test_safe_reciprocal_success (self):
        result = safe_reciprocal(4)
        expected = Success(0.25)
        self.assertEquals(result, expected)

    def test_left_identity_failure (self):
        comp = partial_identity >= safe_root
        nocomp = safe_root
        self.assertEquals(comp(-3), nocomp(-3))

    def test_left_identity_success (self):
        comp = partial_identity >= safe_root
        nocomp = safe_root
        self.assertEquals(comp(100), nocomp(100))

    def test_right_identity_failure (self):
        comp = safe_root >= partial_identity
        nocomp = safe_root
        self.assertEquals(comp(-256), nocomp(-256))

    def test_right_identity_success (self):
        comp = safe_root >= partial_identity
        nocomp = safe_root
        self.assertEquals(comp(256), nocomp(256))

    def test_composition_goodInput (self):
        f = safe_root >= safe_reciprocal
        result = f(25)
        expected = Success(0.2)
        self.assertEquals(result, expected)

    def test_composition_badInput (self):
        f = safe_root >= safe_reciprocal
        result = f(0)
        expected = Failure()
        self.assertEquals(result, expected)

    def test_composition_badReturnType (self):
        const = lambda x: x
        bad = Partial(const)
        self.assertRaises(TypeError, lambda: (bad >= bad)("foo"))
        # TypeError: wrong return type: (str) - should be Success() or Failure()


if __name__ == "__main__":
    unittest.main() # run all tests

