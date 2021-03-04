// Copy Trait

fn main() {
    // Stack
    // - Fixed size known at compile time
    // - Variables are allocated by pushing / popping LIFO
    // - Super fast to allocate
    // - Can not use with dynamic collection, including
    let x: i32 = 10;
    let f: f32 = 2.05;
    let s = "Hello World";

    let y = x;
    let g = f;

    println!("X {} Y {} F {} G {}", x, y, f, g);

    // Heap
    // - Slower to allocate, ask OS. Then get pointer to memory
    // - Lifetime of memory can be independent of the scope that created it
    // - You can dynamically reallocate - allows for dynamic containers
    // - One Allocate means One Free

    let mut string = String::from("Hello World");
    let vector: Vec<i8> = Vec::new();
    let int_heap: Box<i32> = Box::new(15);

    println!("A {} and B {}", string, string_two);
}

