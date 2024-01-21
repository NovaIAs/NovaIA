```go
// Importamos las librerías necesarias.
import (
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"regexp"
	"strconv"
	"strings"
	"time"
)

// Definimos una estructura para almacenar la información de un usuario.
type Usuario struct {
	Nombre      string
	Apellido    string
	Correo      string
	Contraseña  string
	FechaNac    time.Time
	Administrador bool
}

// Creamos una lista de usuarios.
var usuarios = []Usuario{
	{Nombre: "Juan", Apellido: "García", Correo: "juangarcia@example.com", Contraseña: "contraseña1", FechaNac: time.Date(1980, 1, 1), Administrador: false},
	{Nombre: "María", Apellido: "López", Correo: "marialopez@example.com", Contraseña: "contraseña2", FechaNac: time.Date(1985, 2, 1), Administrador: false},
	{Nombre: "Admin", Apellido: "Admin", Correo: "admin@example.com", Contraseña: "contraseña3", FechaNac: time.Date(1970, 3, 1), Administrador: true},
}

// Definimos una función para manejar la solicitud de registro de un usuario.
func registroUsuario(w http.ResponseWriter, r *http.Request) {
	// Si el método de la solicitud es POST, procesamos el registro.
	if r.Method == "POST" {
		// Extraemos los valores del formulario.
		nombre := r.FormValue("nombre")
		apellido := r.FormValue("apellido")
		correo := r.FormValue("correo")
		contraseña := r.FormValue("contraseña")
		fechaNac := r.FormValue("fecha_nac")
		administrador := false

		// Validamos los valores del formulario.
		if nombre == "" || apellido == "" || correo == "" || contraseña == "" || fechaNac == "" {
			http.Error(w, "Todos los campos deben ser rellenados.", http.StatusBadRequest)
			return
		}

		// Validamos el correo electrónico.
		if !validarCorreo(correo) {
			http.Error(w, "Correo electrónico inválido.", http.StatusBadRequest)
			return
		}

		// Validamos la contraseña.
		if !validarContraseña(contraseña) {
			http.Error(w, "Contraseña inválida. Debe tener al menos 8 caracteres, una letra mayúscula, una letra minúscula y un número.", http.StatusBadRequest)
			return
		}

		// Validamos la fecha de nacimiento.
		fechaNacTime, err := time.Parse("2006-01-02", fechaNac)
		if err != nil {
			http.Error(w, "Fecha de nacimiento inválida.", http.StatusBadRequest)
			return
		}

		// Creamos el nuevo usuario.
		nuevoUsuario := Usuario{Nombre: nombre, Apellido: apellido, Correo: correo, Contraseña: contraseña, FechaNac: fechaNacTime, Administrador: administrador}

		// Añadimos el nuevo usuario a la lista de usuarios.
		usuarios = append(usuarios, nuevoUsuario)

		// Redirigimos al usuario a la página principal.
		http.Redirect(w, r, "/", http.StatusSeeOther)
	} else {
		// Si el método de la solicitud no es POST, mostramos el formulario de registro.
		http.ServeFile(w, r, "registro.html")
	}
}

// Definimos una función para manejar la solicitud de inicio de sesión de un usuario.
func inicioSesion(w http.ResponseWriter, r *http.Request) {
	// Si el método de la solicitud es POST, procesamos el inicio de sesión.
	if r.Method == "POST" {
		// Extraemos los valores del formulario.
		correo := r.FormValue("correo")
		contraseña := r.FormValue("contraseña")

		// Validamos el correo electrónico y la contraseña.
		usuario, err := autenticarUsuario(correo, contraseña)
		if err != nil {
			http.Error(w, "Correo electrónico o contraseña incorrectos.", http.StatusUnauthorized)
			return
		}

		// Creamos una sesión para el usuario.
		session, err := sessionStore.New(r, "session")
		if err != nil {
			log.Println(err)
			http.Error(w, "Error al crear la sesión.", http.StatusInternalServerError)
			return
		}

		// Almacenamos el ID del usuario en la sesión.
		session.Values["usuario_id"] = usuario.ID

		// Guardamos la sesión.
		if err := session.Save(r, w); err != nil {
			log.Println(err)
			http.Error(w,