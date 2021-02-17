fn kleisly<A, B, C>(
    f: impl Fn(A) -> Option<B>,
    g: impl Fn(B) -> Option<C>,
) -> impl Fn(A) -> Option<C> {
    move |a: A| f(a).map(|b| g(b)).flatten()
}

fn safe_root(value: f64) -> Option<f64> {
    if (value >= 0.0) {
        Some(value.sqrt())
    } else {
        None
    }
}

fn safe_reciprocal(value: f64) -> Option<f64> {
    if (value != 0.0) {
        Some(1.0 / value)
    } else {
        None
    }
}

fn main() {
    let safe_rootciprocal = kleisly(safe_root, safe_reciprocal);
    assert_ne!(safe_rootciprocal(2.0), None);
    assert_eq!(safe_rootciprocal(0.0), None);
    assert_eq!(safe_rootciprocal(-2.0), None);
}
