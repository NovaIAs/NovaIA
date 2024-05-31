```typescript
// Système de réservation de salle de réunion
// Importer les classes nécessaires
import { Salle, Reunion } from './classes';

// Créer une liste de salles de réunion
let salles: Salle[] = [
  new Salle('Salle A', 10),
  new Salle('Salle B', 15),
  new Salle('Salle C', 20),
];

// Créer une liste de réunions
let reunions: Reunion[] = [
  new Reunion('Réunion 1', '10:00', '12:00', 10),
  new Reunion('Réunion 2', '13:00', '15:00', 15),
  new Reunion('Réunion 3', '16:00', '17:00', 5),
];

// Fonction pour rechercher une salle disponible
function trouverSalleDisponible(capacite: number, heureDebut: string, heureFin: string): Salle | undefined {
  for (let salle of salles) {
    if (salle.capacite >= capacite && salle.estDisponible(heureDebut, heureFin)) {
      return salle;
    }
  }
  return undefined;
}

// Fonction pour réserver une salle
function reserverSalle(salle: Salle, reunion: Reunion): void {
  salle.reserver(reunion);
}

// Fonction pour annuler une réservation
function annulerReservation(salle: Salle, reunion: Reunion): void {
  salle.annulerReservation(reunion);
}

// Fonction principale
function main(): void {
  // Rechercher une salle disponible pour la Réunion 1
  const salleDisponible1 = trouverSalleDisponible(reunions[0].capacite, reunions[0].heureDebut, reunions[0].heureFin);
  if (salleDisponible1) {
    reserverSalle(salleDisponible1, reunions[0]);
    console.log(`Réunion 1 réservée dans la ${salleDisponible1.nom}`);
  } else {
    console.log("Aucune salle disponible pour la Réunion 1");
  }

  // Rechercher une salle disponible pour la Réunion 2
  const salleDisponible2 = trouverSalleDisponible(reunions[1].capacite, reunions[1].heureDebut, reunions[1].heureFin);
  if (salleDisponible2) {
    reserverSalle(salleDisponible2, reunions[1]);
    console.log(`Réunion 2 réservée dans la ${salleDisponible2.nom}`);
  } else {
    console.log("Aucune salle disponible pour la Réunion 2");
  }

  // Rechercher une salle disponible pour la Réunion 3
  const salleDisponible3 = trouverSalleDisponible(reunions[2].capacite, reunions[2].heureDebut, reunions[2].heureFin);
  if (salleDisponible3) {
    reserverSalle(salleDisponible3, reunions[2]);
    console.log(`Réunion 3 réservée dans la ${salleDisponible3.nom}`);
  } else {
    console.log("Aucune salle disponible pour la Réunion 3");
  }

  // Annuler la réservation de la Réunion 1
  annulerReservation(salleDisponible1, reunions[0]);
  console.log(`Réservation de la Réunion 1 annulée`);
}

// Appeler la fonction principale
main();
```

**Explication du code :**

Ce code implémente un système de réservation de salle de réunion. Il permet de créer des salles de réunion, des réunions et de réserver/annuler des salles pour des réunions.

La classe `Salle` représente une salle de réunion et contient des informations telles que le nom et la capacité. La classe `Reunion` représente une réunion et contient des informations telles que le nom, l'heure de début, l'heure de fin et la capacité.

La fonction `trouverSalleDisponible` recherche une salle de réunion disponible pour une réunion donnée, en fonction de la capacité et de l'heure.

La fonction `reserverSalle` réserve une salle pour une réunion donnée.

La fonction `annulerReservation` annule une réservation pour une réunion donnée.

La fonction `main` est la fonction principale qui crée des salles de réunion, des réunions et réserve/annule des salles pour des réunions.