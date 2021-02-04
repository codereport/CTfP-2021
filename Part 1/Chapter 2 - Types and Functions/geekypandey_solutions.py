# Python 3.8

# 1 Memoize function
from functools import lru_cache
from typing import Callable


func = Callable

def memoize_function(f: func) -> func:
    @lru_cache(maxsize=None)
    def g(*args, **kwargs):
        return f(*args, **kwargs)
    return g

# testing memoized function

import operator
import time

from functools import reduce


def fact(n: int) -> int:
    time.sleep(2)
    if n == 0:
        return 1
    return reduce(operator.mul, (i for i in range(1, n+1)), 1)

fact = memoize_function(fact)

def test_memoize_function() -> None:
    print('Testing memoized function...')
    start = time.time()
    x = fact(5)
    end = time.time()
    print(f'First call: {end-start}')

    start = time.time()
    y = fact(5)
    end = time.time()
    print(f'Second call: {end-start}')

test_memoize_function()
# first call takes 2 sec, and second call is done in 10-6 second.


# 2 Memoizing function producing random numbers
# We could memoize the random function but then it would just produce
# one value everytime, which would make it lose its semantic meaning
# of producing random numbers.

# memoizing random
import random


random_memoized = memoize_function(random.random)

def test_random_memoized() -> None:
    x = random_memoized()
    y = random_memoized()
    assert x == y

test_random_memoized()


# 3 Yes, it works. Since we consider seed as the parameter.
# It becomes a mapping from Int -> Float
import random


def random_gen(*, seed: int = None) -> float:
    if seed:
        random.seed(seed)
    return random.random()

random_gen = memoize_function(random_gen)

def test_random_gen() -> None:
    x = random_gen(seed=5)
    y = random_gen(seed=6)
    assert x != y

test_random_gen()

# 4
# a is a pure function, as it can be memoizable.
# b is a pure function, as for given input, produces same output.
# c is a pure function, as it always returns true and prints same thing on terminal.
# d is not a pure function. try calling it with the same value twice, it would yield different output.


# 5: Bool -> Bool we could implement 2 functions.

true = lambda : True
false = lambda : False
