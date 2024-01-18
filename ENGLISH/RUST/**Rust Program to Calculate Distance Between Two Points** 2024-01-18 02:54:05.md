```rust
// This code defines a struct called "Point" with two fields: "x" and "y".
struct Point {
    x: i32,
    y: i32,
}

// This code defines a function called "distance_between_points" that takes two points as arguments and returns the distance between them.
fn distance_between_points(p1: &Point, p2: &Point) -> f64 {
    let dx = (p1.x - p2.x) as f64;
    let dy = (p1.y - p2.y) as f64;
    ((dx * dx) + (dy * dy)).sqrt()
}

// This code defines a function called "main" that is the entry point of the program.
fn main() {
    // This code creates two instances of the "Point" struct.
    let p1 = Point { x: 1, y: 2 };
    let p2 = Point { x: 3, y: 4 };

    // This code calls the "distance_between_points" function with the two points as arguments.
    let distance = distance_between_points(&p1, &p2);

    // This code prints the distance to the console.
    println!("The distance between the two points is: {}", distance);
}
```

Explanation:

1. **Struct Definition**: The `struct Point { x: i32, y: i32, }` defines a new data type called `Point` that has two fields, `x` and `y`, both of type `i32`. This allows us to represent a point in a two-dimensional space.

2. **Function Definition**: The `fn distance_between_points(p1: &Point, p2: &Point) -> f64` defines a function named `distance_between_points` that takes two `Point` references as arguments and returns a `f64` value. This function calculates the distance between the two points.

3. **Function Implementation**: Inside the `distance_between_points` function, we use the Pythagorean theorem to calculate the distance between the two points. We first calculate the difference between the `x` and `y` coordinates of the two points and square them. Then, we add the squared differences and take the square root to find the distance.

4. **Main Function**: The `fn main()` function is the entry point of the program. This is where the execution starts.

5. **Creating Point Instances**: Inside the `main` function, we create two instances of the `Point` struct, `p1` and `p2`, using the curly braces syntax to specify the values for the `x` and `y` fields.

6. **Calling the Function**: We call the `distance_between_points` function with `p1` and `p2` as arguments and store the result in the `distance` variable.

7. **Printing the Distance**: Finally, we use the `println!` macro to print the calculated distance to the console.