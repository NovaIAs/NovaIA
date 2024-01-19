```go
// Crear un programa que gestione una lista de tareas.
// Utiliza una base de datos para almacenar las tareas y un servidor web para interactuar con los usuarios.

package main

import (
	"database/sql"
	"fmt"
	"html/template"
	"log"
	"net/http"
	"os"

	_ "github.com/mattn/go-sqlite3"
)

// Definir la estructura de una tarea.
type Tarea struct {
	id          int
	descripcion  string
	completada  bool
}

// Crear una conexión a la base de datos.
func connectDB() (*sql.DB, error) {
	db, err := sql.Open("sqlite3", "./tareas.db")
	if err != nil {
		return nil, err
	}
	return db, nil
}

// Crear una tabla de tareas si no existe.
func createTable(db *sql.DB) error {
	query := `
		CREATE TABLE IF NOT EXISTS tareas (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			descripcion TEXT NOT NULL,
			completada BOOL NOT NULL DEFAULT FALSE
		);
	`
	_, err := db.Exec(query)
	return err
}

// Insertar una tarea en la base de datos.
func insertTask(db *sql.DB, tarea Tarea) (int, error) {
	query := `
		INSERT INTO tareas (descripcion, completada)
		VALUES (?, ?);
	`
	result, err := db.Exec(query, tarea.descripcion, tarea.completada)
	if err != nil {
		return 0, err
	}
	id, err := result.LastInsertId()
	return int(id), err
}

// Obtener todas las tareas de la base de datos.
func getAllTasks(db *sql.DB) ([]Tarea, error) {
	query := `
		SELECT id, descripcion, completada
		FROM tareas;
	`
	rows, err := db.Query(query)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	var tareas []Tarea
	for rows.Next() {
		var tarea Tarea
		if err := rows.Scan(&tarea.id, &tarea.descripcion, &tarea.completada); err != nil {
			return nil, err
		}
		tareas = append(tareas, tarea)
	}
	return tareas, nil
}

// Actualizar una tarea en la base de datos.
func updateTask(db *sql.DB, tarea Tarea) error {
	query := `
		UPDATE tareas
		SET descripcion = ?,
		completada = ?
		WHERE id = ?;
	`
	_, err := db.Exec(query, tarea.descripcion, tarea.completada, tarea.id)
	return err
}

// Eliminar una tarea de la base de datos.
func deleteTask(db *sql.DB, id int) error {
	query := `
		DELETE FROM tareas
		WHERE id = ?;
	`
	_, err := db.Exec(query, id)
	return err
}

// Crear un manejador para la página principal.
func homeHandler(w http.ResponseWriter, r *http.Request) {
	db, err := connectDB()
	if err != nil {
		http.Error(w, "Error al conectar con la base de datos.", http.StatusInternalServerError)
		return
	}
	defer db.Close()

	if err := createTable(db); err != nil {
		http.Error(w, "Error al crear la tabla de tareas.", http.StatusInternalServerError)
		return
	}

	tareas, err := getAllTasks(db)
	if err != nil {
		http.Error(w, "Error al obtener las tareas de la base de datos.", http.StatusInternalServerError)
		return
	}

	t, err := template.ParseFiles("templates/home.html")
	if err != nil {
		http.Error(w, "Error al parsear la plantilla.", http.StatusInternalServerError)
		return
	}

	if err := t.Execute(w, tareas); err != nil {
		http.Error(w, "Error al mostrar la página principal.", http.StatusInternalServerError)
		return
	}
}

// Crear un manejador para el formulario de creación de tareas.
func createTaskHandler(w http.ResponseWriter, r *http.Request) {
	if r.Method == http.POST {
		descripcion := r.FormValue("descripcion")

		db, err := connectDB()
		if err != nil {
			http.Error(w, "Error al conectar con la base de datos.", http.StatusInternalServerError)
			return
		}
		defer db.Close()

		if err := createTable(db); err != nil {
			http.Error(w, "Error al crear la tabla de tareas.", http.StatusInternalServerError)
			return
		}

		tarea := Tarea{descripcion: descripcion, completada: false}
		id, err := insertTask(db, tarea)
		if err != nil {
			http.Error(w, "Error al insertar la tarea en la base de datos.", http.StatusInternalServerError)
			return
		}

		http.Redirect(w, r, "/", http.StatusSeeOther)
	} else {
		t, err := template.ParseFiles("templates/create.html")
		if err != nil {
			http.Error(w, "Error al parsear la plantilla.", http.StatusInternalServerError)
			return
		}

		if err := t.Execute(w, nil); err != nil {
			http.Error(w, "Error al mostrar el formulario de creación de tareas.", http.StatusInternalServerError)
			return
		}
	}
}

// Crear un manejador para el formulario de edición de tareas.
func editTaskHandler(w http.ResponseWriter, r *http.Request) {
	id := r.URL.Query().Get("id")

	db, err := connectDB()
	if err != nil {
		http.Error(w, "Error al conectar con la base de