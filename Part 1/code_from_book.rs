
// Chapter 2: Types and Functions

// https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=38808d9c1e43468901d4e9e167291deb

fn fact(n: i32) -> i32 {
    (1..=n).product()
}
