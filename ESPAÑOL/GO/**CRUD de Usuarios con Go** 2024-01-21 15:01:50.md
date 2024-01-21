```go
package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"strconv"
	"sync"
	"time"

	"github.com/go-chi/chi/v5"
	"github.com/go-playground/validator/v10"
)

// Estructura para almacenar los datos de un usuario
type Usuario struct {
	ID       uint64 `json:"id"`
	Nombre   string `json:"nombre" validate:"required"`
	Correo   string `json:"correo" validate:"required,email"`
	Password string `json:"password" validate:"required"`
}

// Repositorio de usuarios en memoria
type RepositorioUsuarios struct {
	usuarios []*Usuario
	mutex    *sync.Mutex
}

// Variable global para el repositorio de usuarios
var repositorioUsuarios RepositorioUsuarios

// Función para inicializar el repositorio de usuarios
func init() {
	repositorioUsuarios = RepositorioUsuarios{
		usuarios: make([]*Usuario, 0),
		mutex:    &sync.Mutex{},
	}
}

// Función para crear un usuario
func crearUsuario(w http.ResponseWriter, r *http.Request) {
	var usuario Usuario

	// Decodifica el cuerpo de la petición en la variable usuario
	if err := json.NewDecoder(r.Body).Decode(&usuario); err != nil {
		http.Error(w, "Error al decodificar el cuerpo de la petición", http.StatusBadRequest)
		return
	}

	// Valida los datos del usuario
	if err := validator.New().Struct(usuario); err != nil {
		http.Error(w, "Error al validar los datos del usuario", http.StatusBadRequest)
		return
	}

	// Genera un nuevo ID para el usuario
	usuario.ID = uint64(len(repositorioUsuarios.usuarios)) + 1

	// Bloquea el acceso al repositorio de usuarios
	repositorioUsuarios.mutex.Lock()
	defer repositorioUsuarios.mutex.Unlock()

	// Añade el usuario al repositorio
	repositorioUsuarios.usuarios = append(repositorioUsuarios.usuarios, &usuario)

	// Devuelve el usuario creado
	w.WriteHeader(http.StatusCreated)
	if err := json.NewEncoder(w).Encode(usuario); err != nil {
		http.Error(w, "Error al codificar la respuesta", http.StatusInternalServerError)
		return
	}
}

// Función para obtener todos los usuarios
func obtenerUsuarios(w http.ResponseWriter, r *http.Request) {
	// Bloquea el acceso al repositorio de usuarios
	repositorioUsuarios.mutex.RLock()
	defer repositorioUsuarios.mutex.RUnlock()

	// Devuelve todos los usuarios
	w.WriteHeader(http.StatusOK)
	if err := json.NewEncoder(w).Encode(repositorioUsuarios.usuarios); err != nil {
		http.Error(w, "Error al codificar la respuesta", http.StatusInternalServerError)
		return
	}
}

// Función para obtener un usuario por su ID
func obtenerUsuarioPorID(w http.ResponseWriter, r *http.Request) {
	id, err := strconv.ParseUint(chi.URLParam(r, "id"), 10, 64)
	if err != nil {
		http.Error(w, "Error al parsear el ID del usuario", http.StatusBadRequest)
		return
	}

	// Bloquea el acceso al repositorio de usuarios
	repositorioUsuarios.mutex.RLock()
	defer repositorioUsuarios.mutex.RUnlock()

	// Busca el usuario por su ID
	for _, usuario := range repositorioUsuarios.usuarios {
		if usuario.ID == id {
			w.WriteHeader(http.StatusOK)
			if err := json.NewEncoder(w).Encode(usuario); err != nil {
				http.Error(w, "Error al codificar la respuesta", http.StatusInternalServerError)
				return
			}
			return