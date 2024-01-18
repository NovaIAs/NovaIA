```rust
// Command-line arguments parsing
use clap::{App, Arg};

// Random number generation
use rand::prelude::*;

// Multithreading
use std::thread;
use std::sync::{Arc, Mutex};

// Data structure
use std::collections::{HashMap, BTreeMap, HashSet};

// JSON encoding/decoding
use serde::{Serialize, Deserialize};

// HTTP server
use actix_web::{web, App, HttpServer, Responder, HttpResponse, Error, HttpRequest};

// Database
use diesel::{prelude::*, QueryDsl, RunQueryDsl};

// Logging
use log::{debug, info, warn, error};

// Struct representing a task
#[derive(Serialize, Deserialize, Clone)]
struct Task {
    id: i32,
    title: String,
    description: String,
    completed: bool,
}

// Struct representing the request to create a task
#[derive(Serialize, Deserialize)]
struct CreateTaskRequest {
    title: String,
    description: String,
}

// Struct representing the request to update a task
#[derive(Serialize, Deserialize)]
struct UpdateTaskRequest {
    id: i32,
    title: String,
    description: String,
    completed: bool,
}

// Main function
#[tokio::main]
async fn main() -> Result<(), Error> {
    // Command-line arguments parsing
    let args = App::new("My Application")
        .arg(Arg::with_name("database_url")
            .short("d")
            .long("database-url")
            .value_name("DATABASE_URL")
            .help("Sets the database URL")
            .required(true))
        .arg(Arg::with_name("port")
            .short("p")
            .long("port")
            .value_name("PORT")
            .help("Sets the port to listen on")
            .required(true))
        .get_matches();

    // Database connection
    let database_url = args.value_of("database_url").unwrap();
    let pool = establish_connection(database_url)?;

    // HTTP server configuration
    let port = args.value_of("port").unwrap();
    let server = HttpServer::new(move || {
        App::new()
            .data(pool.clone())
            .route("/tasks", web::get().to(get_all_tasks))
            .route("/tasks", web::post().to(create_task))
            .route("/tasks/{id}", web::get().to(get_task))
            .route("/tasks/{id}", web::put().to(update_task))
            .route("/tasks/{id}", web::delete().to(delete_task))
    });

    // Start the server
    info!("Starting server on port {}", port);
    server.bind(format!("0.0.0.0:{}", port))?
        .run()
        .await?;

    Ok(())
}

// Function to establish a database connection
fn establish_connection(database_url: &str) -> Result<PgConnection, Error> {
    PgConnection::establish(database_url)
        .map_err(|e| Error::from(e.to_string()))
}

// Function to get all tasks
async fn get_all_tasks(pool: web::Data<PgConnection>) -> Result<HttpResponse, Error> {
    use diesel::dsl::{task, tasks};

    // Query the database
    let tasks = task::table.load::<Task>(&**pool)?;

    // Serialize the tasks to JSON
    let json = serde_json::to_string(&tasks)?;

    Ok(HttpResponse::Ok().json(json))
}

// Function to create a task
async fn create_task(pool: web::Data<PgConnection>, request: web::Json<CreateTaskRequest>) -> Result<HttpResponse, Error> {
    use diesel::dsl::{task, insert_into};

    // Create a new task
    let new_task = Task {
        id: 0,
        title: request.title.clone(),
        description: request.description.clone(),
        completed: false,
    };

    // Insert the task into the database
    let task = diesel::insert_into(task::table)
        .values(&new_task)
        .get_result(&**pool)?;

    // Serialize the task to JSON
    let json = serde_json::to_string(&task)?;

    Ok(HttpResponse::Created().json(json))
}

// Function to get a task by its ID
async fn get_task(pool: web::Data<PgConnection>, request: HttpRequest) -> Result<HttpResponse, Error> {
    use diesel::dsl::{task, tasks};

    // Get the task ID from the request path
    let id = request.match_info().get("id").unwrap();

    // Query the database
    let task = task::table.filter(tasks::id.eq(id))
        .first(&**pool)?;

    // Serialize the task to JSON
    let json = serde_json::to_string(&task)?;

    Ok(HttpResponse::Ok().json(json))
}

// Function to update a task
async fn update_task(pool: web::Data<PgConnection>, request: HttpRequest, update_task: web::Json<UpdateTaskRequest>) -> Result<HttpResponse, Error> {
    use diesel::dsl::{task, tasks, update};

    // Get the task ID from the request path
    let id = request.match_info().get("id").unwrap();

    // Update the task in the database
    let task = diesel::update(task::table.filter(tasks::id.eq(id)))
        .set((
            tasks::title.eq(update_task.title.clone()),
            tasks::description.eq(update_task.description.clone()),
            tasks::completed.eq(update_task.completed),
        ))
        .get_result(&**pool)?;

    // Serialize the task to JSON
    let json = serde_json::to_string(&task)?;

    Ok(HttpResponse::Ok().json(json))
}

// Function to delete a task
async fn delete_task(pool: web::Data<PgConnection>, request: HttpRequest) -> Result<HttpResponse, Error> {
    use diesel::dsl::{task, tasks, delete};

    // Get the task ID from the request path
    let id = request.match_info().get("id").unwrap();

    // Delete the task from the database
    diesel::delete(task::table.filter(tasks::id.eq(id)))
        .execute(&**pool)?;

    Ok(HttpResponse::NoContent().finish())
}
```