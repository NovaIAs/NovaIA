```rust
// This Rust code demonstrates a complex and differentiated program that showcases the versatility and capabilities of the language.

// Importing necessary modules and libraries.
use std::io;
use std::collections::HashMap;
use std::fs::File;
use std::path::Path;
use std::io::prelude::*;

// Defining a custom data structure for representing a task.
#[derive(Clone, Debug)]
struct Task {
    title: String,
    description: String,
    priority: u8,
    due_date: String,
    status: String,
}

// Creating a function to read tasks from a text file.
fn read_tasks_from_file(file_path: &str) -> Result<Vec<Task>, io::Error> {
    let path = Path::new(file_path);
    let mut file = match File::open(&path) {
        Ok(file) => file,
        Err(error) => return Err(error),
    };

    let mut tasks: Vec<Task> = Vec::new();
    let mut buffer = String::new();
    while file.read_line(&mut buffer)? > 0 {
        let task_data: Vec<&str> = buffer.split(',').collect();
        if task_data.len() == 5 {
            tasks.push(Task {
                title: task_data[0].to_string(),
                description: task_data[1].to_string(),
                priority: task_data[2].parse::<u8>()?,
                due_date: task_data[3].to_string(),
                status: task_data[4].to_string(),
            });
        }
        buffer.clear();
    }

    Ok(tasks)
}

// Creating a function to save tasks to a text file.
fn save_tasks_to_file(file_path: &str, tasks: &Vec<Task>) -> Result<(), io::Error> {
    let path = Path::new(file_path);
    let mut file = match File::create(&path) {
        Ok(file) => file,
        Err(error) => return Err(error),
    };

    for task in tasks {
        let task_data = format!(
            "{},{},{},{},{}\n",
            task.title, task.description, task.priority, task.due_date, task.status
        );
        file.write_all(task_data.as_bytes())?;
    }

    Ok(())
}

// Creating a function to display the main menu options to the user.
fn display_main_menu() {
    println!("\n--- Task Manager Menu ---");
    println!("1. Add a new task");
    println!("2. View all tasks");
    println!("3. Edit a task");
    println!("4. Delete a task");
    println!("5. Mark a task as complete");
    println!("6. Exit");
    print!("Enter your choice: ");
}

// Creating a function to handle user input and call appropriate functions based on the input.
fn handle_user_input(tasks: &mut Vec<Task>) {
    loop {
        display_main_menu();
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        let choice: u8 = input.trim().parse().unwrap();

        match choice {
            1 => add_new_task(tasks),
            2 => view_all_tasks(tasks),
            3 => edit_task(tasks),
            4 => delete_task(tasks),
            5 => mark_task_as_complete(tasks),
            6 => {
                println!("Exiting the program...");
                break;
            }
            _ => println!("Invalid choice. Please enter a number between 1 and 6."),
        }
    }
}

// Creating a function to add a new task to the task list.
fn add_new_task(tasks: &mut Vec<Task>) {
    println!("\n--- Add a New Task ---");
    let mut title = String::new();
    println!("Enter the task title: ");
    io::stdin().read_line(&mut title).unwrap();
    title = title.trim().to_string();

    let mut description = String::new();
    println!("Enter the task description: ");
    io::stdin().read_line(&mut description).unwrap();
    description = description.trim().to_string();

    let mut priority: u8;
    loop {
        println!("Enter the task priority (1-5): ");
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        priority = input.trim().parse().unwrap();
        if priority >= 1 && priority <= 5 {
            break;
        } else {
            println!("Invalid priority. Please enter a number between 1 and 5.");
        }
    }

    let mut due_date = String::new();
    println!("Enter the task due date (YYYY-MM-DD): ");
    io::stdin().read_line(&mut due_date).unwrap();
    due_date = due_date.trim().to_string();

    let status = "In Progress".to_string();

    tasks.push(Task {
        title,
        description,
        priority,
        due_date,
        status,
    });

    println!("New task added successfully!");
}

// Creating a function to view all tasks in the task list.
fn view_all_tasks(tasks: &Vec<Task>) {
    println!("\n--- View All Tasks ---");
    if tasks.is_empty() {
        println!("No tasks found.");
    } else {
        println!("{:?}", tasks);
    }
}

// Creating a function to edit an existing task.
fn edit_task(tasks: &mut Vec<Task>) {
    println!("\n--- Edit a Task ---");
    let mut id: usize;
    loop {
        println!("Enter the ID of the task you want to edit: ");
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        id = input.trim().parse().unwrap();
        if id < tasks.len() {
            break;
        } else {
            println!("Invalid ID. Please enter a valid ID.");
        }
    }

    let task = &mut tasks[id];

    println!("Enter the new task title (or leave blank to keep the current title): ");
    let mut new_title = String::new();
    io::stdin().read_line(&mut new_title).unwrap();
    new_title = new_title.trim().to_string();
    if !new_title.is_empty() {
        task.title = new_title;
    }

    println!("Enter the new task description (or leave blank to keep the current description): ");
    let mut new_description = String::new();
    io::stdin().read_line(&mut new_description).unwrap();
    new_description = new_description.trim().to_string();
    if !new_description.is_empty() {
        task.description = new_description;
    }

    let mut new_priority: u8;
    loop {
        println!("Enter the new task priority (1-5) (or leave blank to keep the current priority): ");
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        if input.trim().is_empty() {
            break;
        }
        new_priority = input.trim().parse().unwrap();
        if new_priority >= 1 && new_priority <= 5 {
            task.priority = new_priority;
            break;
        } else {
            println!("Invalid priority. Please enter a number between 1 and 5.");
        }
    }

    println!("Enter the new task due date (YYYY-MM-DD) (or leave blank to keep the current due date): ");
    let mut new_due_date = String::new();
    io::stdin().read_line(&mut new_due_date).unwrap();
    new_due_date = new_due_date.trim().to_string();
    if !new_due_date.is_empty() {
        task.due_date = new_due_date;
    }

    println!("Task updated successfully!");
}

// Creating a function to delete a task from the task list.
fn delete_task(tasks: &mut Vec<Task>) {
    println!("\n--- Delete a Task ---");
    let mut id: usize;
    loop {
        println!("Enter the ID of the task you want to delete: ");
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        id = input.trim().parse().unwrap