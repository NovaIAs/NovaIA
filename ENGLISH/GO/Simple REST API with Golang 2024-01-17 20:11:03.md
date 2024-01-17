```go
package main

import (
	"errors"
	"fmt"
	"io"
	"log"
	"math"
	"math/rand"
	"net/http"
	"os"
	"reflect"
	"regexp"
	"runtime"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/gorilla/mux"
	"github.com/json-iterator/go"
)

// Person is a simple struct representing a person.
type Person struct {
	ID        int    `json:"id,omitempty"`
	FirstName string `json:"firstName"`
	LastName  string `json:"lastName"`
	Age       int    `json:"age"`
}

// PersonService defines the methods required for a person service.
type PersonService interface {
	GetPeople() ([]Person, error)
	GetPerson(id int) (Person, error)
	CreatePerson(p Person) (Person, error)
	UpdatePerson(p Person) (Person, error)
	DeletePerson(id int) error
}

// PersonServiceImpl is a simple implementation of the PersonService interface.
type PersonServiceImpl struct {
	mux sync.Mutex
	p   []Person
}

// GetPeople returns a list of all persons.
func (s *PersonServiceImpl) GetPeople() ([]Person, error) {
	s.mux.Lock()
	defer s.mux.Unlock()
	return s.p, nil
}

// GetPerson returns a single person by its ID.
func (s *PersonServiceImpl) GetPerson(id int) (Person, error) {
	s.mux.Lock()
	defer s.mux.Unlock()
	for _, p := range s.p {
		if p.ID == id {
			return p, nil
		}
	}
	return Person{}, errors.New("person not found")
}

// CreatePerson creates a new person.
func (s *PersonServiceImpl) CreatePerson(p Person) (Person, error) {
	s.mux.Lock()
	defer s.mux.Unlock()
	p.ID = len(s.p) + 1
	s.p = append(s.p, p)
	return p, nil
}

// UpdatePerson updates an existing person.
func (s *PersonServiceImpl) UpdatePerson(p Person) (Person, error) {
	s.mux.Lock()
	defer s.mux.Unlock()
	for i, pp := range s.p {
		if pp.ID == p.ID {
			s.p[i] = p
			return p, nil
		}
	}
	return Person{}, errors.New("person not found")
}

// DeletePerson deletes a person by its ID.
func (s *PersonServiceImpl) DeletePerson(id int) error {
	s.mux.Lock()
	defer s.mux.Unlock()
	for i, p := range s.p {
		if p.ID == id {
			s.p = append(s.p[:i], s.p[i+1:]...)
			return nil
		}
	}
	return errors.New("person not found")
}

// NewPersonService creates a new PersonServiceImpl.
func NewPersonService() PersonService {
	return &PersonServiceImpl{
		p: []Person{
			{1, "John", "Doe", 30},
			{2, "Jane", "Doe", 25},
			{3, "Peter", "Parker", 20},
		},
	}
}

// JSONError is the struct used for JSON error responses.
type JSONError struct {
	Message string `json:"message"`
}

// JSONResponse is the struct used for JSON responses.
type JSONResponse struct {
	Data interface{} `json:"data,omitempty"`
}

// App is the main application struct.
type App struct {
	router *mux.Router
	ps     PersonService
}

// NewApp creates a new App.
func NewApp(ps PersonService) *App {
	a := &App{
		router: mux.NewRouter(),
		ps:     ps,
	}
	a.routes()
	return a
}

// ServeHTTP implements the http.Handler interface.
func (a *App) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	a.router.ServeHTTP(w, r)
}

// routes defines the application routes.
func (a *App) routes() {
	a.router.HandleFunc("/people", a.getPeople).Methods("GET")
	a.router.HandleFunc("/people/{id}", a.getPerson).Methods("GET")
	a.router.HandleFunc("/people", a.createPerson).Methods("POST")
	a.router.HandleFunc("/people/{id}", a.updatePerson).Methods("PUT")
	a.router.HandleFunc("/people/{id}", a.deletePerson).Methods("DELETE")
}

// getPeople handles the request to get all people.
func (a *App) getPeople(w http.ResponseWriter, r *http.Request) {
	people, err := a.ps.GetPeople()
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	JSONResponse{Data: people}.WriteJSON(w)
}

// getPerson handles the request to get a single person.
func (a *App) getPerson(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	id, err := strconv.Atoi(vars["id"])
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}
	person, err := a.ps.GetPerson(id)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	JSONResponse{Data: person}.WriteJSON(w)
}

// createPerson handles the request to create a new person.
func (a *App) createPerson(w http.ResponseWriter, r *http.Request) {
	var p Person
	if err := json.NewDecoder(r.Body).Decode(&p); err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}
	person, err := a.ps.CreatePerson(p)
	if err != nil {
		http.Error(w, err.Error