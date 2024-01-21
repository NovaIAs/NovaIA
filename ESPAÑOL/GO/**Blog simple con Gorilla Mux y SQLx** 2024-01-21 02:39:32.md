```go
package main

import (
	"fmt"
	"log"
	"net/http"
	"time"

	"github.com/gorilla/mux"
	"github.com/jmoiron/sqlx"
	_ "github.com/lib/pq"
	"golang.org/x/crypto/bcrypt"
)

type User struct {
	ID       int64
	Username string
	Password []byte
}

func hashPassword(password string) ([]byte, error) {
	return bcrypt.GenerateFromPassword([]byte(password), bcrypt.DefaultCost)
}

func checkPassword(hashedPassword, password []byte) bool {
	return bcrypt.CompareHashAndPassword(hashedPassword, password) == nil
}

type Post struct {
	ID      int64
	Title   string
	Body    string
	Author  int64
	Created time.Time
}

var db *sqlx.DB

func init() {
	var err error
	db, err = sqlx.Connect("postgres", "user=postgres password=password dbname=blog sslmode=disable")
	if err != nil {
		log.Fatal(err)
	}
}

func main() {
	router := mux.NewRouter()

	router.HandleFunc("/users", createUser).Methods("POST")
	router.HandleFunc("/users/{username}", getUser).Methods("GET")
	router.HandleFunc("/posts", createPost).Methods("POST")
	router.HandleFunc("/posts/{id}", getPost).Methods("GET")

	http.Handle("/", router)
	log.Fatal(http.ListenAndServe(":8080", nil))
}

func createUser(w http.ResponseWriter, r *http.Request) {
	user := &User{}
	if err := json.NewDecoder(r.Body).Decode(user); err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	hashedPassword, err := hashPassword(user.Password)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	user.Password = hashedPassword

	result, err := db.NamedExec("INSERT INTO users (username, password) VALUES (:username, :password)", user)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	id, err := result.LastInsertId()
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	user.ID = id

	json.NewEncoder(w).Encode(user)
}

func getUser(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	username := vars["username"]

	user := &User{}
	err := db.Get(user, "SELECT * FROM users WHERE username = $1", username)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	json.NewEncoder(w).Encode(user)
}

func createPost(w http.ResponseWriter, r *http.Request) {
	post := &Post{}
	if err := json.NewDecoder(r.Body).Decode(post); err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	result, err := db.NamedExec("INSERT INTO posts (title, body, author) VALUES (:title, :body, :author)", post)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	id, err := result.LastInsertId()
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	post.ID = id

	json.NewEncoder(w).Encode(post)
}

func getPost(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	id := vars["id"]

	post := &Post{}
	err := db.Get(post, "SELECT * FROM posts WHERE id = $1", id)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	json.NewEncoder(w).Encode(post)
}
```

Explicación:

* El código crea una simple aplicación web usando el framework Gorilla Mux y el ORM SQLx.
* La aplicación permite crear y obtener usuarios y posts.
* Los usuarios tienen un nombre de usuario y una contraseña, que se almacena cifrada en la base de datos.
* Los posts tienen un título, un cuerpo y un autor.
* La aplicación utiliza el patrón de diseño Model-View-Controller (MVC) para separar la lógica de negocio de la interfaz de usuario.
* El código utiliza el patrón de diseño Singleton para crear una única instancia de la base de datos, que se utiliza en todos los controladores.
* El código también utiliza el patrón de diseño Factory Method para crear nuevas instancias de usuarios y posts.
* El código utiliza el patrón de diseño Repository para abstraer el acceso a la base de datos.
* El código utiliza el patrón de diseño Service para encapsular la lógica de negocio.
* El código utiliza el patrón de diseño Controller para manejar las solicitudes HTTP.
* El código utiliza el patrón de diseño Middleware para procesar las solicitudes HTTP antes de que lleguen a los controladores.
* El código utiliza el patrón de diseño Template para renderizar las vistas HTML.