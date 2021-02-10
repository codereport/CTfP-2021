
// Chapter 2: Types and Functions

// https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=38808d9c1e43468901d4e9e167291deb

fn fact(n: i32) -> i32 {
    (1..=n).product()
}

// Chapter 3: Categories Great and Small

// https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=423070c111fa47c3ec4a8b5f62bde454

fn sum(n: i32) -> i32 {
    (1..=n).fold(0, |a, b| a + b )
}

fn product(n: i32) -> i32 {
    (1..=n).fold(1, |a, b| a * b )
}
