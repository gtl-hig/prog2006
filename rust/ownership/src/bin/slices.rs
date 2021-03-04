fn main() {
    let s = "I have four words".to_owned();
    let mut v = vec![2, 6, 4, 5, 6, 7, 8, 9];

    println!("Before: {:?}", v);
    multiply_vector(&mut v[2..5], 10);
    println!("After: {:?}", v);

    let first = first_odd_number_out(&v).unwrap();
    println!("First odd: {:?}", first);
    v.clear();
}

fn multiply_vector(v: &mut [i32], factor: i32) {
    for val in v {
        *val = *val * factor;
    }
}

fn first_odd_number_out(v: &[i32]) -> Option<&[i32]> {
    for (i, val) in v.iter().enumerate() {
        if val % 2 == 1 {
            return Some(&v[i..]);
        }
    }
    None
}