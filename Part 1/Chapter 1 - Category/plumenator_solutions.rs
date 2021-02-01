// 1.

#[allow(dead_code)]
fn id_owned<T>(thing: T) -> T {
    thing
}

#[allow(dead_code)]
fn id_referenced<T: Clone>(thing: &T) -> T {
    thing.clone()
}

// 2.

#[allow(dead_code)]
fn compose<A, B, C>(f: impl Fn(B) -> C, g: impl Fn(A) -> B) -> impl Fn(A) -> C {
    Box::new(move |x| f(g(x)))
}

// 3.

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compose_id() {
        let f = |x| 2 * x;
        let f_dot_id = compose(f, id_owned);
        let id_dot_f = compose(id_owned, f);
        assert_eq!(f(2), f_dot_id(2));
        assert_eq!(f(2), id_dot_f(2));
    }
}

// 4. No, the web is not a category. A page can link to itself
// (identity), but you cannot componse links.

// 5. No, Facebook is not a category with people as objects and
// friendships as morphisms. My friends of friends are not my friends.

// 6. A directed graph is a category if each node points to itself,
// perhaps in addition to others.
