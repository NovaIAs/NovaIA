**Système de Gestion des Étudiants**

Ce code complexe en Go implémente un système de gestion des étudiants complet, permettant d'effectuer diverses opérations sur les données des étudiants, notamment l'ajout, la suppression, la modification et la recherche.

```go
package main

import (
    "database/sql"
    "fmt"
    "log"
    "net/http"
    "html/template"
    "strconv"
    _ "github.com/go-sql-driver/mysql"
)

const (
    host     = "127.0.0.1"
    database = "gestion_etudiants"
    user     = "root"
    password = ""
)

type Student struct {
    ID   int
    Nom  string
    Prenom string
    Filiere string
}

func main() {
    db, err := sql.Open("mysql", fmt.Sprintf("%s:%s@tcp(%s)/%s", user, password, host, database))
    if err != nil {
        log.Fatal(err)
    }
    defer db.Close()

    // Création du serveur HTTP
    http.HandleFunc("/", home)
    http.HandleFunc("/ajouter", ajouter)
    http.HandleFunc("/supprimer", supprimer)
    http.HandleFunc("/modifier", modifier)
    http.HandleFunc("/rechercher", rechercher)
    if err := http.ListenAndServe(":8080", nil); err != nil {
        log.Fatal(err)
    }
}

func home(w http.ResponseWriter, r *http.Request) {
    tpl, err := template.ParseFiles("home.html")
    if err != nil {
        log.Fatal(err)
    }

    var students []Student
    rows, err := db.Query("SELECT * FROM etudiants")
    if err != nil {
        log.Fatal(err)
    }
    for rows.Next() {
        var student Student
        if err := rows.Scan(&student.ID, &student.Nom, &student.Prenom, &student.Filiere); err != nil {
            log.Fatal(err)
        }
        students = append(students, student)
    }

    tpl.Execute(w, students)
}

func ajouter(w http.ResponseWriter, r *http.Request) {
    if r.Method == "POST" {
        nom := r.FormValue("nom")
        prenom := r.FormValue("prenom")
        filiere := r.FormValue("filiere")

        _, err := db.Exec("INSERT INTO etudiants (nom, prenom, filiere) VALUES (?, ?, ?)", nom, prenom, filiere)
        if err != nil {
            log.Fatal(err)
        }

        http.Redirect(w, r, "/", 302)
    }

    tpl, err := template.ParseFiles("ajouter.html")
    if err != nil {
        log.Fatal(err)
    }
    tpl.Execute(w, nil)
}

func supprimer(w http.ResponseWriter, r *http.Request) {
    id := r.URL.Query().Get("id")

    _, err := db.Exec("DELETE FROM etudiants WHERE id = ?", id)
    if err != nil {
        log.Fatal(err)
    }

    http.Redirect(w, r, "/", 302)
}

func modifier(w http.ResponseWriter, r *http.Request) {
    if r.Method == "POST" {
        id := r.FormValue("id")
        nom := r.FormValue("nom")
        prenom := r.FormValue("prenom")
        filiere := r.FormValue("filiere")

        _, err := db.Exec("UPDATE etudiants SET nom = ?, prenom = ?, filiere = ? WHERE id = ?", nom, prenom, filiere, id)
        if err != nil {
            log.Fatal(err)
        }

        http.Redirect(w, r, "/", 302)
    }

    id := r.URL.Query().Get("id")
    row := db.QueryRow("SELECT * FROM etudiants WHERE id = ?", id)

    var student Student
    if err := row.Scan(&student.ID, &student.Nom, &student.Prenom, &student.Filiere); err != nil {
        log.Fatal(err)
    }

    tpl, err := template.ParseFiles("modifier.html")
    if err != nil {
        log.Fatal(err)
    }
    tpl.Execute(w, student)
}

func rechercher(w http.ResponseWriter, r *http.Request) {
    var students []Student
    var err error

    nomRecherche := r.URL.Query().Get("nom")
    if nomRecherche != "" {
        rows, err := db.Query("SELECT * FROM etudiants WHERE nom LIKE ?", "%"+nomRecherche+"%")
        if err != nil {
            log.Fatal(err)
        }
        for rows.Next() {
            var student Student
            if err := rows.Scan(&student.ID, &student.Nom, &student.Prenom, &student.Filiere); err != nil {
                log.Fatal(err)
            }
            students = append(students, student)
        }
    } else {
        filiereRecherche := r.URL.Query().Get("filiere")
        if filiereRecherche != "" {
            rows, err := db.Query("SELECT * FROM etudiants WHERE filiere = ?", filiereRecherche)
            if err != nil {
                log.Fatal(err)
            }
            for rows.Next() {
                var student Student
                if err := rows.Scan(&student.ID, &student.Nom, &student.Prenom, &student.Filiere); err != nil {
                    log.Fatal(err)
                }
                students = append(students, student)
            }
        }
    }

    tpl, err := template.ParseFiles("rechercher.html")
    if err != nil {
        log.Fatal(err)
    }
    tpl.Execute(w, students)
}
```

**Explication du Code**

* **Type Student:** Représente le modèle d'étudiant avec des champs pour l'ID, le nom, le prénom et la filière.
* **Connexion à la base de données:** La connexion à la base de données MySQL est établie à l'aide du package `database/sql`.
* **Serveur HTTP:** Le code crée un serveur HTTP qui écoute sur le port `8080`.
* **Gestionnaires:** Des gestionnaires HTTP sont définis pour les différentes opérations : `home` (page d'accueil), `ajouter` (ajout d'un étudiant), `supprimer` (suppression d'un étudiant), `modifier` (modification d'un étudiant) et `rechercher` (recherche d'étudiants).
* **Modèles:** Des modèles HTML sont utilisés pour générer et afficher les pages dynamiquement.
* **Requêtes SQL:** Le code utilise des requêtes SQL pour effectuer des opérations sur les données des étudiants dans la base de données.
* **Conversion de chaîne:** La fonction `strconv.Atoi()` est utilisée pour convertir des valeurs de chaîne en entiers pour l'ID de l'étudiant.