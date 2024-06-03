**Un système de réservation d'hôtel avancé en Dart**

Ce code implémente un système complet de réservation d'hôtel avec des fonctionnalités avancées telles que la gestion des chambres, la recherche de disponibilité, la réservation, l'annulation et la modification des réservations. Voici le code :

```dart
import 'dart:math';

class Hotel {
  String nom;
  List<Chambre> chambres;

  Hotel(this.nom, this.chambres);

  // Trouve des chambres disponibles pour les dates données
  List<Chambre> trouverDisponibilites(DateTime arrivee, DateTime depart) {
    return chambres.where((c) => c.estDisponible(arrivee, depart)).toList();
  }

  // Crée une nouvelle réservation
  Reservation reserver(Chambre chambre, Client client, DateTime arrivee, DateTime depart) {
    chambre.reserver(client, arrivee, depart);
    return Reservation(client, chambre, arrivee, depart);
  }

  // Annule une réservation existante
  void annulerReservation(Reservation reservation) {
    reservation.chambre.annuler();
  }

  // Modifie une réservation existante
  void modifierReservation(Reservation reservation, DateTime nouvelleArrivee, DateTime nouveauDepart) {
    reservation.chambre.annuler();
    reservation.chambre.reserver(reservation.client, nouvelleArrivee, nouveauDepart);
  }
}

class Chambre {
  int numero;
  bool occupe;
  Reservation? reservation;

  Chambre(this.numero, {this.occupe = false, this.reservation});

  // Vérifie si la chambre est disponible pour les dates données
  bool estDisponible(DateTime arrivee, DateTime depart) {
    return !occupe && (reservation == null || reservation.dateDepart < arrivee || reservation.dateArrivee > depart);
  }

  // Réserve la chambre pour les dates données
  void reserver(Client client, DateTime arrivee, DateTime depart) {
    reservation = Reservation(client, this, arrivee, depart);
    occupe = true;
  }

  // Annule la réservation de la chambre
  void annuler() {
    reservation = null;
    occupe = false;
  }
}

class Reservation {
  Client client;
  Chambre chambre;
  DateTime dateArrivee;
  DateTime dateDepart;

  Reservation(this.client, this.chambre, this.dateArrivee, this.dateDepart);
}

class Client {
  String nom;
  String email;

  Client(this.nom, this.email);
}

void main() {
  // Créer un hôtel avec des chambres
  var hotel = Hotel("Grand Hôtel", [
    Chambre(101),
    Chambre(102),
    Chambre(103),
  ]);

  // Trouver les chambres disponibles pour les dates données
  var chambresDisponibles = hotel.trouverDisponibilites(DateTime.now(), DateTime.now().add(Duration(days: 2)));

  // Réserver une chambre
  var reservation = hotel.reserver(chambresDisponibles[0], Client("John Doe", "johndoe@example.com"), DateTime.now(), DateTime.now().add(Duration(days: 1)));

  // Modifier la réservation
  hotel.modifierReservation(reservation, DateTime.now().add(Duration(days: 1)), DateTime.now().add(Duration(days: 3)));

  // Annuler la réservation
  hotel.annulerReservation(reservation);
}
```

**Explication du code :**

* La classe `Hotel` représente un hôtel et contient une liste de chambres.
* La classe `Chambre` représente une chambre d'hôtel et gère les réservations.
* La classe `Reservation` représente une réservation d'hôtel.
* La classe `Client` représente un client de l'hôtel.
* La méthode `main` crée un hôtel, trouve les chambres disponibles, fait une réservation, change la réservation et annule finalement la réservation.

Ce code implémente un système de réservation d'hôtel complet et flexible qui peut être étendu pour prendre en charge des fonctionnalités supplémentaires, telles que les paiements, les notifications par e-mail et l'intégration avec des systèmes tiers.