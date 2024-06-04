**Gestion d'un système de réservation de billets de train en Go:**

```go
package main

import (
    "database/sql"
    "fmt"
    "log"
    "time"

    _ "github.com/go-sql-driver/mysql"
)

const (
    DBUser     = "utilisateur"
    DBPassword = "motdepasse"
    DBPort     = 3306
    DBHost     = "localhost"
    DBName     = "reservations"
)

type Train struct {
    ID       int64
    Name     string
    Capacity int
}

type Ticket struct {
    ID         int64
    TrainID    int64
    Passenger  string
    Departure  time.Time
    Arrival    time.Time
    SeatNumber int
}

func main() {
    db, err := sql.Open("mysql", fmt.Sprintf("%s:%s@tcp(%s:%d)/%s?parseTime=true", DBUser, DBPassword, DBHost, DBPort, DBName))
    if err != nil {
        log.Fatalf("Erreur de connexion à la base de données : %v", err)
    }
    defer db.Close()

    _, err = db.Exec(`
        CREATE TABLE IF NOT EXISTS trains (
            id INT NOT NULL AUTO_INCREMENT,
            name VARCHAR(50) NOT NULL,
            capacity INT NOT NULL,
            PRIMARY KEY (id)
        )
    `)
    if err != nil {
        log.Fatalf("Erreur de création de la table 'trains' : %v", err)
    }

    _, err = db.Exec(`
        CREATE TABLE IF NOT EXISTS tickets (
            id INT NOT NULL AUTO_INCREMENT,
            train_id INT NOT NULL,
            passenger VARCHAR(50) NOT NULL,
            departure DATETIME NOT NULL,
            arrival DATETIME NOT NULL,
            seat_number INT NOT NULL,
            PRIMARY KEY (id),
            FOREIGN KEY (train_id) REFERENCES trains(id)
        )
    `)
    if err != nil {
        log.Fatalf("Erreur de création de la table 'tickets' : %v", err)
    }

    // Insérer des trains de démonstration
    insertTrain(db, "TGV Duplex", 800)
    insertTrain(db, "Intercités", 400)
    insertTrain(db, "TER", 200)

    // Afficher les trains disponibles
    fmt.Println("Trains disponibles :")
    trains, err := getTrains(db)
    if err != nil {
        log.Fatalf("Erreur de récupération des trains : %v", err)
    }
    for _, train := range trains {
        fmt.Printf("%d - %s (%d places)\n", train.ID, train.Name, train.Capacity)
    }

    // Réserver un billet
    fmt.Printf("Entrez l'ID du train que vous souhaitez réserver : ")
    var trainID int64
    fmt.Scanf("%d", &trainID)

    train, err := getTrain(db, trainID)
    if err != nil {
        log.Fatalf("Erreur de récupération du train : %v", err)
    }

    fmt.Printf("Entrez le nom du passager : ")
    var passenger string
    fmt.Scanf("%s", &passenger)

    fmt.Printf("Entrez la date et l'heure de départ (format AAAA-MM-JJ HH:MM) : ")
    var departure string
    fmt.Scanf("%s", &departure)
    d, err := time.Parse("2006-01-02 15:04", departure)
    if err != nil {
        log.Fatalf("Erreur de parsing de la date et de l'heure de départ : %v", err)
    }

    fmt.Printf("Entrez la date et l'heure d'arrivée (format AAAA-MM-JJ HH:MM) : ")
    var arrival string
    fmt.Scanf("%s", &arrival)
    a, err := time.Parse("2006-01-02 15:04", arrival)
    if err != nil {
        log.Fatalf("Erreur de parsing de la date et de l'heure d'arrivée : %v", err)
    }

    fmt.Printf("Entrez le numéro du siège : ")
    var seatNumber int
    fmt.Scanf("%d", &seatNumber)

    ticket, err := createTicket(db, train.ID, passenger, d, a, seatNumber)
    if err != nil {
        log.Fatalf("Erreur de création du ticket : %v", err)
    }

    fmt.Printf("Billet réservé avec succès !\nBillet n° %d\n", ticket.ID)
}

func insertTrain(db *sql.DB, name string, capacity int) error {
    _, err := db.Exec("INSERT INTO trains (name, capacity) VALUES (?, ?)", name, capacity)
    return err
}

func getTrains(db *sql.DB) ([]Train, error) {
    rows, err := db.Query("SELECT * FROM trains")
    if err != nil {
        return nil, err
    }
    defer rows.Close()

    var trains []Train
    for rows.Next() {
        var train Train
        if err := rows.Scan(&train.ID, &train.Name, &train.Capacity); err != nil {
            return nil, err
        }
        trains = append(trains, train)
    }
    return trains, nil
}

func getTrain(db *sql.DB, id int64) (Train, error) {
    var train Train
    err := db.QueryRow("SELECT * FROM trains WHERE id = ?", id).Scan(&train.ID, &train.Name, &train.Capacity)
    return train, err
}

func createTicket(db *sql.DB, trainID int64, passenger string, departure time.Time, arrival time.Time, seatNumber int) (Ticket, error) {
    res, err := db.Exec("INSERT INTO tickets (train_id, passenger, departure, arrival, seat_number) VALUES (?, ?, ?, ?, ?)", trainID, passenger, departure, arrival, seatNumber)
    if err != nil {
        return Ticket{}, err
    }
    id, err := res.LastInsertId()
    if err != nil {
        return Ticket{}, err
    }
    return Ticket{ID: id, TrainID: trainID, Passenger: passenger, Departure: departure, Arrival: arrival, SeatNumber: seatNumber}, nil
}
```

**Explication du code :**

* Le code se connecte à une base de données MySQL et crée les tables `trains` et `tickets` si elles n'existent pas.
* Il insère des trains de démonstration dans la table `trains`.
* L'utilisateur peut ensuite choisir un train, fournir les informations du passager et réserver un billet.
* Le code utilise `fmt.Scanf` pour lire les entrées de l'utilisateur.
* Il utilise `time.Parse` pour parser les chaînes de date et d'heure en objets `time.Time`.
* Le code utilise les méthodes Query et QueryRow de l'objet `sql.DB` pour exécuter des requêtes SQL et récupérer les résultats.
* Il utilise les méthodes `Prepare` et `Exec` de l'objet `sql.Stmt` pour insérer de nouvelles données dans la base de données.
* Le code utilise `res.LastInsertId` pour récupérer l'ID du ticket nouvellement créé.