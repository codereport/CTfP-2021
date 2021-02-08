// 1.
// Mostly copied from https://medium.com/swlh/on-memoization-291fd1dd924

use std::collections::HashMap;
use std::hash::Hash;

pub struct MemoFunc<A, R> {
    memo: HashMap<A, R>,
    func: fn(&mut MemoFunc<A, R>, A) -> R,
}

impl<A, R> MemoFunc<A, R>
where
    A: Eq + Hash + Clone,
    R: Clone,
{
    pub fn from_func(func: fn(&mut Self, A) -> R) -> Self {
        Self {
            memo: HashMap::new(),
            func,
        }
    }

    pub fn call(&mut self, arg: A) -> R {
        let func = self.func.clone();
        if let Some(ret) = self.memo.get(&arg) {
            ret.clone()
        } else {
            let ret = func(self, arg.clone());
            self.memo.insert(arg, ret.clone());
            ret
        }
    }
}

pub struct Plain<A, R>(fn(&mut Plain<A, R>, A) -> R);

impl<A, R> Plain<A, R> {
    pub fn call(&mut self, arg: A) -> R {
        (self.0)(self, arg)
    }
}

#[cfg(test)]
mod test_1 {
    use super::*;

    #[test]
    fn test_fib() {
        fn fib<M>(memo_func: &mut M, n: u64) -> u64 {
            match n {
                0 => 0,
                1 => 1,
                n => fib(memo_func, n - 1) + fib(memo_func, n - 1),
            }
        }

        assert_eq!(Plain(fib).call(5), MemoFunc::from_func(fib).call(5));
        assert_eq!(Plain(fib).call(10), MemoFunc::from_func(fib).call(10));

        use std::time::Instant;

        let mut memo_fib = MemoFunc::from_func(fib);
        memo_fib.call(24); // populate the cache
        let now = Instant::now();
        memo_fib.call(25);
        let memo_elapsed = now.elapsed();

        let now = Instant::now();
        Plain(fib).call(25);
        let plain_elapsed = now.elapsed();

        // This actually fails right now :-(
        assert!(memo_elapsed < plain_elapsed);
    }
}

#[cfg(test)]
mod test_2 {
    use super::*;

    #[test]
    fn test_rand() {
        // 2.

        use rand::{thread_rng, Rng}; // 0.8.2
        fn random<M>(_: &mut M, _: ()) -> f64 {
            let mut rng = thread_rng();
            rng.gen::<f64>()
        }

        assert_ne!(Plain(random).call(()), MemoFunc::from_func(random).call(()));

        // It doesn't work

        // 3.

        use rand::{rngs::StdRng, SeedableRng}; // 0.8.2
        fn seeded<M>(_: &mut M, seed: [u8; 32]) -> f64 {
            let mut rng = StdRng::from_seed(seed);
            rng.gen::<f64>()
        }
        let mut seed = [0u8; 32];
        thread_rng().fill(&mut seed[..]);
        assert_eq!(
            Plain(seeded).call(seed),
            MemoFunc::from_func(seeded).call(seed)
        );

        // It works
    }
}

// 5.

fn always_true(_: bool) -> bool {
    true
}

fn always_false(_: bool) -> bool {
    false
}

fn id(b: bool) -> bool {
    b
}

fn not(b: bool) -> bool {
    !b
}

// 6.

// !, (), bool

fn true_f(_: ()) -> bool {
    true
}
// or
fn false_f(_: ()) -> bool {
    false
}

fn never_unit(_: ()) -> ! {
    panic!()
}

fn unit_f(_: bool) -> () {
    ()
}

fn never_b(_: bool) -> ! {
    panic!()
}

// essentially the same as !
enum Never {}

fn true_never(_: Never) -> bool {
    true
}
// or
fn false_never(_: Never) -> bool {
    false
}

fn unit_never(_: Never) -> () {
    ()
}
