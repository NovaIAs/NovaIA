```rust
// This is a complex Rust code that demonstrates a variety of programming concepts.

// Import necessary libraries.
use std::io;
use std::collections::HashMap;
use std::thread;
use std::sync::{Arc, Mutex};

// Define a struct to represent a task.
#[derive(Clone)]
struct Task {
    id: u32,
    name: String,
    duration: u32,
}

// Define a function to generate a random task.
fn generate_task() -> Task {
    Task {
        id: rand::random(),
        name: format!("Task {}", rand::random()),
        duration: rand::random_range(1, 10),
    }
}

// Define a function to run a task.
fn run_task(task: Task) {
    println!("Running task {}: {}", task.id, task.name);
    thread::sleep(std::time::Duration::from_secs(task.duration));
    println!("Task {} completed.", task.id);
}

// Define a function to manage a pool of tasks.
fn manage_task_pool(tasks: &mut Vec<Task>) {
    // Create a shared state to store the status of tasks.
    let task_status: Arc<Mutex<HashMap<u32, bool>>> = Arc::new(Mutex::new(HashMap::new()));

    // Create a pool of threads to run tasks concurrently.
    let mut threads = Vec::new();
    for task in tasks.iter() {
        let task_status_clone = task_status.clone();
        let task_clone = task.clone();
        threads.push(thread::spawn(move || {
            run_task(task_clone);

            // Update the task status in the shared state.
            let mut task_status_lock = task_status_clone.lock().unwrap();
            task_status_lock.insert(task_clone.id, true);
        }));
    }

    // Wait for all threads to complete.
    for thread in threads {
        thread.join().unwrap();
    }

    // Print the status of all tasks.
    let task_status_lock = task_status.lock().unwrap();
    for (id, status) in task_status_lock.iter() {
        println!("Task {}: {}", id, if *status { "Completed" } else { "Failed" });
    }
}

// Get user input for the number of tasks to generate.
println!("Enter the number of tasks to generate:");
let mut input = String::new();
io::stdin().read_line(&mut input).unwrap();
let num_tasks: u32 = input.trim().parse().unwrap();

// Generate a vector of tasks.
let mut tasks: Vec<Task> = Vec::with_capacity(num_tasks as usize);
for _ in 0..num_tasks {
    tasks.push(generate_task());
}

// Manage the task pool and print the status of all tasks.
manage_task_pool(&mut tasks);
```

This code demonstrates the following concepts:

* **Struct**: The `Task` struct is used to represent a task with its id, name, and duration.
* **Function**: The `generate_task()` function is used to generate a random task.
* **Thread**: The `run_task()` function is used to run a task in a separate thread.
* **Mutex**: The `Arc<Mutex<HashMap<u32, bool>>>` is used to share the status of tasks between threads.
* **Thread Pool**: The `manage_task_pool()` function creates a pool of threads to run tasks concurrently.
* **I/O**: The `io::stdin().read_line()` function is used to get user input.
* **Parsing**: The `parse()` function is used to parse the user input into a number.
* **Vector**: The `Vec<Task>` is used to store the generated tasks.
* **Loop**: The `for` loop is used to generate multiple tasks.
* **Conditional Statement**: The `if` statement is used to check the status of a task.

This code is a complex example that demonstrates a variety of programming concepts in Rust. It is not intended to be a practical program, but rather a showcase of the language's features.