//
//  code_chapter_2.swift
//  cli_testing
//
//  Created by Sky Shaver on 2021-02-07.
//

import Foundation

// Question 1
// not completed, do not share :)

func fib_m(_ n: Int) -> Int {
    var fibs: [Int] = [1, 1]
    (2...n).forEach { i in
        fibs.append(fibs[i - 1] + fibs[i - 2])
    }
    return fibs.last!
}

// not memoized on 1500 elements of 5 ,6 ,7 repeated 0.0134 s.
func no_memo<T: Hashable>(_ arr: [T], _ f: (T) -> T) -> [T] {
    return arr.map{f($0)};
}

// memoized on 1500 elements of 5 ,6 ,7 repeated 0.0011s
func memo<T: Hashable>(_ arr: [T], _ f: (T) -> T) -> [T] {
    // have to use ternary operator or function on rhs is called every time
    let memo_map = arr.reduce(into: [T: T]()){$0[$1] = ($0[$1] != nil) ? $0[$1] : f($1)}
    return arr.map{memo_map[$0]!};
}

// function that takes a pure function and returns a memoized function
// basically returns the above function
func memoize<T: Hashable>(_ arr: [T], _ f: @escaping (T) -> T) -> () -> [T] {
    return {
        let memo_map = arr.reduce(into: [T: T]()){$0[$1] = ($0[$1] != nil) ? $0[$1] : f($1)}
        return arr.map{memo_map[$0]!};
    }
}
