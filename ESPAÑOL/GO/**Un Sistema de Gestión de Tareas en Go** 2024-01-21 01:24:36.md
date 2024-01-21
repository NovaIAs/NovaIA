```go
// Package mipaquete proporciona una biblioteca de lógica de negocios compleja en Go.
package mipaquete

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"strings"
	"time"

	"github.com/google/uuid"
)

// Tarea representa una tarea que se puede realizar.
type Tarea struct {
	ID          uuid.UUID `json:"id"`
	Descripcion string    `json:"descripcion"`
	FechaLimite time.Time `json:"fecha_limite"`
	Completado  bool      `json:"completado"`
}

// NuevaTarea crea una nueva tarea.
func NuevaTarea(descripcion string, fechaLimite time.Time) Tarea {
	return Tarea{
		ID:          uuid.New(),
		Descripcion: descripcion,
		FechaLimite: fechaLimite,
		Completado:  false,
	}
}

// CompletarTarea marca una tarea como completada.
func (t *Tarea) CompletarTarea() {
	t.Completado = true
}

// Tareas es una colección de tareas.
type Tareas struct {
	Tareas []Tarea `json:"tareas"`
}

// AñadirTarea añade una tarea a la colección.
func (ts *Tareas) AñadirTarea(tarea Tarea) {
	ts.Tareas = append(ts.Tareas, tarea)
}

// ObtenerTareas devuelve todas las tareas de la colección.
func (ts *Tareas) ObtenerTareas() []Tarea {
	return ts.Tareas
}

// ObtenerTareasCompletadas devuelve todas las tareas completadas de la colección.
func (ts *Tareas) ObtenerTareasCompletadas() []Tarea {
	var tareasCompletadas []Tarea
	for _, tarea := range ts.Tareas {
		if tarea.Completado {
			tareasCompletadas = append(tareasCompletadas, tarea)
		}
	}
	return tareasCompletadas
}

// ObtenerTareasPendientes devuelve todas las tareas pendientes de la colección.
func (ts *Tareas) ObtenerTareasPendientes() []Tarea {
	var tareasPendientes []Tarea
	for _, tarea := range ts.Tareas {
		if !tarea.Completado {
			tareasPendientes = append(tareasPendientes, tarea)
		}
	}
	return tareasPendientes
}

// ServidorHTTP es un servidor HTTP que puede utilizarse para gestionar tareas.
type ServidorHTTP struct {
	Tareas *Tareas
}

// NuevoServidorHTTP crea un nuevo servidor HTTP.
func NuevoServidorHTTP(tareas *Tareas) *ServidorHTTP {
	return &ServidorHTTP{
		Tareas: tareas,
	}
}

// Servir inicia el servidor HTTP.
func (s *ServidorHTTP) Servir(puerto string) error {
	http.HandleFunc("/tareas", s.tareasHandler)
	return http.ListenAndServe(puerto, nil)
}

// tareasHandler es el controlador para la ruta "/tareas".
func (s *ServidorHTTP) tareasHandler(w http.ResponseWriter, r *http.Request) {
	switch r.Method {
	case http.MethodGET:
		s.obtenerTareasHandler(w, r)
	case http.MethodPOST:
		s.crearTareaHandler(w, r)
	case http.MethodPUT:
		s.actualizarTareaHandler(w, r)
	case http.MethodDELETE:
		s.eliminarTareaHandler(w, r)
	default:
		http.Error(w, "Método no permitido", http.StatusMethodNotAllowed)
	}
}

// obtenerTareasHandler es el controlador para la ruta "/tareas".
func (s *ServidorHTTP) obtenerTareasHandler(w http.ResponseWriter, r *http.Request) {
	tareas := s.Tareas.ObtenerTareas()
	b, err := json.Marshal(tareas)
	if err != nil {
		http.Error(w, "Error al codificar JSON", http.StatusInternalServerError)
		return
	}
	w.Header().Set("Content-Type", "application/json")
	w.Write(b)
}

// crearTareaHandler es el controlador para la ruta "/tareas".
func (s *ServidorHTTP) crearTareaHandler(w http.ResponseWriter, r *http.Request) {
	var tarea Tarea
	if err := json.NewDecoder(r.Body).Decode(&tarea); err != nil {
		http.Error(w, "Error al decodificar JSON", http.StatusBadRequest)
		return
	}
	s.Tareas.AñadirTarea(tarea)
	b, err := json.Marshal(tarea)
	if err != nil {
		http.Error(w, "Error al codificar JSON", http.StatusInternalServerError)
		return
	}
	w.Header().Set("Content-Type", "application/json")
	w.Write(b)
}

// actualizarTareaHandler es el controlador para la ruta "/tareas".
func (s *ServidorHTTP) actualizarTareaHandler(w http.ResponseWriter, r *http.Request) {
	var tarea Tarea
	if err := json.NewDecoder(r.Body).Decode(&tarea); err != nil {
		http.Error(w, "Error al decodificar JSON", http.StatusBadRequest)
		return
	}
	for i, t := range s.Tareas.Tareas {
		if t.ID == tarea.ID {
			s.Tareas.Tareas[i] = tarea
			break
		}
	}
	b, err := json.Marshal(tarea)
	if err != nil {
		http.Error(w, "Error al codificar JSON", http.StatusInternalServerError)
		return
	}
	w.Header().Set("Content-Type", "application/json")
	w.Write(b)
}

// eliminarTareaHandler es el controlador para la ruta "/tareas".
func (s *ServidorHTTP) eliminarTareaHandler(w http.ResponseWriter, r *http.Request) {
	id := strings.TrimPrefix(r.URL.Path, "/tareas/")
	uuid, err := uuid.Parse(id)
	if err != nil {
		http.Error(w, "ID de tarea no válido", http.StatusBadRequest)
