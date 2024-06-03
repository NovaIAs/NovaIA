```go
// Code complexe en GO

// Importer les packages nécessaires
package main

import (
    "fmt"
    "io"
    "log"
    "net"
    "net/http"
    "os"
    "path/filepath"
    "strings"
    "sync"
    "time"

    "github.com/gorilla/mux"
    "github.com/joho/godotenv"
    "github.com/lib/pq"
)

// Structures et constantes utilisées dans le code

// Constantes pour les messages de log
const (
    InfoLog  = "INFO"
    WarningLog  = "WARNING"
    ErrorLog  = "ERROR"
    FatalLog  = "FATAL"
)

// Structure pour stocker les informations sur un utilisateur
type User struct {
    ID          int
    Username    string
    Email       string
    Password    string
    Role        string
    LastLogin   time.Time
    CreationDate time.Time
}

// Interface pour définir les méthodes nécessaires pour interagir avec la base de données
type DatabaseHandler interface {
    Open(connectionString string) error
    Close() error
    GetUser(username string) (*User, error)
    CreateUser(user *User) error
    UpdateUser(user *User) error
    DeleteUser(username string) error
}

// Implementation de l'interface DatabaseHandler à l'aide de la bibliothèque pq
type PostgresHandler struct {
    db *pq.DB
}

// Fonctions pour interagir avec la base de données

// Ouvrir la connexion à la base de données
func (h *PostgresHandler) Open(connectionString string) error {
    db, err := pq.Open(connectionString)
    if err != nil {
        return err
    }

    h.db = db
    return nil
}

// Fermer la connexion à la base de données
func (h *PostgresHandler) Close() error {
    return h.db.Close()
}

// Obtenir un utilisateur à partir de la base de données
func (h *PostgresHandler) GetUser(username string) (*User, error) {
    query := "SELECT * FROM users WHERE username = $1"
    var user User
    err := h.db.QueryRow(query, username).Scan(&user.ID, &user.Username, &user.Email, &user.Password, &user.Role, &user.LastLogin, &user.CreationDate)
    if err != nil {
        return nil, err
    }

    return &user, nil
}

// Créer un utilisateur dans la base de données
func (h *PostgresHandler) CreateUser(user *User) error {
    query := "INSERT INTO users (username, email, password, role, last_login, creation_date) VALUES ($1, $2, $3, $4, $5, $6)"
    _, err := h.db.Exec(query, user.Username, user.Email, user.Password, user.Role, user.LastLogin, user.CreationDate)
    if err != nil {
        return err
    }

    return nil
}

// Mettre à jour un utilisateur dans la base de données
func (h *PostgresHandler) UpdateUser(user *User) error {
    query := "UPDATE users SET email = $1, password = $2, role = $3, last_login = $4 WHERE username = $5"
    _, err := h.db.Exec(query, user.Email, user.Password, user.Role, user.LastLogin, user.Username)
    if err != nil {
        return err
    }

    return nil
}

// Supprimer un utilisateur de la base de données
func (h *PostgresHandler) DeleteUser(username string) error {
    query := "DELETE FROM users WHERE username = $1"
    _, err := h.db.Exec(query, username)
    if err != nil {
        return err
    }

    return nil
}

// Fonctions pour gérer les requêtes HTTP

// HandleUserRequest gère les requêtes HTTP pour les opérations sur les utilisateurs
func HandleUserRequest(w http.ResponseWriter, r *http.Request) {
    // Extraire l'action de la requête
    action := mux.Vars(r)["action"]

    // Initialiser le gestionnaire de base de données
    dbHandler := &PostgresHandler{}

    // Ouvrir la connexion à la base de données
    err := dbHandler.Open(connectionString)
    if err != nil {
        http.Error(w, "Impossible de se connecter à la base de données", http.StatusInternalServerError)
        log.Printf("%s: %v", ErrorLog, err)
        return
    }

    // Fermer la connexion à la base de données à la fin de la requête
    defer dbHandler.Close()

    // Effectuer l'action en fonction de la requête
    switch action {
    case "get":
        // Récupérer le nom d'utilisateur à partir de la requête
        username := r.FormValue("username")

        // Récupérer l'utilisateur de la base de données
        user, err := dbHandler.GetUser(username)
        if err != nil {
            http.Error(w, "Impossible de récupérer l'utilisateur", http.StatusInternalServerError)
            log.Printf("%s: %v", ErrorLog, err)
            return
        }

        // Écrire les informations de l'utilisateur dans la réponse
        fmt.Fprintf(w, "ID: %d\nNom d'utilisateur: %s\nEmail: %s\nRôle: %s\nDernière connexion: %s\nDate de création: %s", user.ID, user.Username, user.Email, user.Role, user.LastLogin, user.CreationDate)

    case "create":
        // Lire les informations de l'utilisateur dans la requête
        username := r.FormValue("username")
        email := r.FormValue("email")
        password := r.FormValue("password")
        role := r.FormValue("role")

        // Créer l'utilisateur
        user := &User{
            Username:    username,
            Email:       email,
            Password:    password,
            Role:        role,
            LastLogin:   time.Now(),
            CreationDate: time.Now(),
        }

        // Créer l'utilisateur dans la base de données
        err = dbHandler.CreateUser(user)
        if err != nil {
            http.Error(w, "Impossible de créer l'utilisateur", http.StatusInternalServerError)
            log.Printf("%s: %v", ErrorLog, err)
            return
        }

        // Écrire un message de réussite dans la réponse
        fmt.Fprintln(w, "Utilisateur créé avec succès")

    case "update":
        // Lire les informations de l'utilisateur dans la requête
        username := r.FormValue("username")
        email := r.FormValue("email")
        password := r.FormValue("password")
        role := r.FormValue("role")

        // Mettre à jour l'utilisateur
        user := &User{
            Username:    username,
            Email:       email,
            Password:    password,
            Role:        role,
            LastLogin:   time.Now(),
        }

        // Mettre à jour l'utilisateur dans la base de données
        err = dbHandler.UpdateUser(user)
        if err != nil {
            http.Error(w, "Impossible de mettre à jour l'utilisateur", http.StatusInternalServerError)
            log.Printf("%s: %v", ErrorLog, err)
            return
        }

        // Écrire un message de réussite dans la réponse
        fmt.Fprintln(w, "Utilisateur mis à jour avec succès")

    case "delete":
        // Récupérer le nom d'utilisateur à partir de la requête
        username := r.FormValue("username")

        // Supprimer l'utilisateur de la base de données
        err = dbHandler.DeleteUser(username)
        if err != nil {
            http.Error(w, "Impossible de supprimer l'utilisateur", http.StatusInternalServerError)
            log.Printf("%s: %v", ErrorLog, err)
            return
        }

        // Écrire un message de réussite dans la réponse
        fmt.Fprintln(w, "Utilisateur supprimé avec succès")

    default:
        // Gérer les actions non reconnues
        http.Error(w, "Action non reconnue", http.StatusBadRequest)
        log.Printf("%s: Action non reconnue: %s", WarningLog, action)
    }
}

// LoggingMiddleware est un middleware pour la journalisation des requêtes HTTP
func LoggingMiddleware(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        // Déterminer le niveau de log
        level := InfoLog
        if strings.Contains(r.URL.Path, "/error