/* Challenge 1. ------*/

// Memoize combinator
function memoize(f) {
    let mappings = {};
    return (...args) => {
        if(mappings[args]) {
            return mappings[args];
        }
        else {
            const result = f(...args);
            mappings[args] = result;
            return result;
        }
    }
}

// Naive fibonacci (function that takes a long time)
function fib(n) {
    switch(n) {
        case 0:
            return 0;
        case 1:
            return 1;
        case 2:
            return 1;
        default:
            return fib(n - 1) + fib(n - 2);
    }
}


const memoFib = memoize(fib);
const inNum = 20;

console.time('First fib run');
const fibResult = memoFib(inNum); // 1.974ms
console.timeEnd('First fib run');
console.log('Result: ', fibResult);


console.time('Second fib run');
const secondFibResult = memoFib(inNum); // 0.012ms
console.timeEnd('Second fib run');
console.log('Result: ', secondFibResult);


/* Challenge 2. ----- */

const memoRandom = memoize(Math.random);
console.log('Memozided random num: ', memoRandom());
console.log('Memozided random num: ', memoRandom());
console.log('Memozided random num: ', memoRandom());// Returns the same output every time


/* Challenge 3. ----- */
// Unfortunately there is no way to seed the Math.random function.
// If there were, memoizing it would work.



/* Challenge 4. ---- */

/*
    (a) Pure
    (b) Not pure
    (c) Not pure (side effects)
    (d) Not pure (different every call)
 */

/* Challenge 5. ---- */

function not(b) {
    return !b;
}

function boolId(b) {
    return !!b;
}

console.log("Boolean 'not': ", not(true)) // false
console.log("Boolean 'id': ", boolId(true)) // true