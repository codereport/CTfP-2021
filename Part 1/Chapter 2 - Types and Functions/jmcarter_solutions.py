import random


##################
# Challenges 2.7 #
##################


#  2.7.1 : define higher-order function "memoize"
def memoize(fn):
    cache = {}

    def memoized(n):
        if n not in cache:
            cache[n] = fn(n)
        return cache[n]

    return memoized


#  2.7.2 : memoize random
def test_memoized_random():
    memo_randrange = memoize(random.randrange)
    value1 = memo_randrange(100)
    value2 = memo_randrange(100)

    assert value1 == value2


#  2.7.3 : memoize random with seed
def test_memoized_random_with_seed():
    def random_with_seed(seed):
        random.seed(seed)
        return random.random()

    val1 = random_with_seed(10)
    val2 = random_with_seed(10)
    assert val1 == val2

    memo_rws = memoize(random_with_seed)
    val1 = memo_rws(10)
    val2 = memo_rws(10)
    assert val1 == val2


#  2.7.4 : which of these are pure :
#  a) factorial :: pure
#  b) getchar(c) :: not pure
#  c) bool f() { std::cout << "Hello\n"; return true; } :: not pure (the print will only occur the first time)
#  d) static var in function changes every call, so not pure


#  2.7.5 : Implement all functions from Bool to Bool
bool_identity = lambda x: x
bool_inverse = lambda x: not x
bool_true = lambda x: True
bool_false = lambda x: False


def test_all_bool_functions():
    assert bool_identity(True) is True
    assert bool_identity(False) is False

    assert bool_inverse(False) is True
    assert bool_inverse(True) is False

    assert bool_true(False) is True
    assert bool_true(True) is True

    assert bool_false(False) is False
    assert bool_false(True) is False


#  2.7.6
#  ---------------------  #
#  | Void        Unit  |  #
#  |                   |  #
#  |       Bool        |  #
#  ---------------------  #
#  id :: Void -> Void
#  absurd :: Void -> Unit
#  absurd :: Void -> Bool
#  id :: Unit -> Unit
#  true :: Unit -> Bool
#  false :: Unit -> Bool
#  id :: Bool -> Bool
#  not :: Bool -> Bool
#  true :: Bool -> Bool
#  false :: Bool -> Bool
#  unit :: Bool -> Unit

def test_void_unit_bool_category():
    # def absurd(void): return any
    id = bool_identity
    true = bool_true
    false = bool_false
    NOT = bool_inverse
    unit = lambda x: None

    assert true(None) is True
    assert false(None) is False
    assert NOT(True) is False
    assert NOT(False) is True
    assert unit(True) is None
