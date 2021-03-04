#[derive(Debug, Clone, Copy)]
struct UserData {
    age: i32,
    max_age: i32
}

impl UserData {
    fn add_age(&mut self, delta: i32) {
        self.age += delta;
    }
}

fn main() {
    let user = UserData{ age: 0, max_age: 90 };
    let user_two = user;

    println!("{:?} and {:?}", user, user_two);
}