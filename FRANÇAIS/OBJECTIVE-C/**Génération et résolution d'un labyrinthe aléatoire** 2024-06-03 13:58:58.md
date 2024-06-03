**Objectif du code:** Générer un labyrinthe aléatoire à l'aide d'un algorithme de génération de labyrinthes récursif, le stocke dans un tableau et permet à un utilisateur de le résoudre.

**Code:**

```objective-c
// Bibliothèque standard C
#include <stdio.h>

// Bibliothèque pour les nombres aléatoires
#include <stdlib.h>

// Bibliothèque pour les structures de données
#include <stdbool.h>

// Taille du labyrinthe
#define TAILLE 10

// Mur nord, est, sud, ouest
#define NORD 1
#define EST 2
#define SUD 4
#define OUEST 8

// Structure représentant une cellule du labyrinthe
typedef struct Cellule {
    bool murs; // Murs de la cellule (nord, est, sud, ouest)
    bool visite; // Si la cellule a été visitée
} Cellule;

// Tableau représentant le labyrinthe
Cellule labyrinthe[TAILLE][TAILLE];

// Fonction pour générer un labyrinthe aléatoire
void genererLabyrinthe() {
    // Initialiser les cellules
    for (int i = 0; i < TAILLE; i++) {
        for (int j = 0; j < TAILLE; j++) {
            labyrinthe[i][j].murs = NORD | EST | SUD | OUEST;
            labyrinthe[i][j].visite = false;
        }
    }

    // Pile pour stocker les cellules à visiter
    Cellule *pile[TAILLE * TAILLE];
    int sommetPile = 0;

    // Cellule de départ aléatoire
    int ligneDepart = rand() % TAILLE;
    int colonneDepart = rand() % TAILLE;

    // Empiler la cellule de départ
    pile[sommetPile++] = &labyrinthe[ligneDepart][colonneDepart];

    // Générer le labyrinthe
    while (sommetPile > 0) {
        // Désempiler la cellule actuelle
        Cellule *celluleCourante = pile[--sommetPile];

        // Marquer la cellule comme visitée
        celluleCourante->visite = true;

        // Liste des murs possibles à supprimer
        int mursPossibles[4];
        int nombreMursPossibles = 0;

        // Nord
        if (celluleCourante->ligne - 1 >= 0 && !labyrinthe[celluleCourante->ligne - 1][celluleCourante->colonne].visite) {
            mursPossibles[nombreMursPossibles++] = NORD;
        }

        // Est
        if (celluleCourante->colonne + 1 < TAILLE && !labyrinthe[celluleCourante->ligne][celluleCourante->colonne + 1].visite) {
            mursPossibles[nombreMursPossibles++] = EST;
        }

        // Sud
        if (celluleCourante->ligne + 1 < TAILLE && !labyrinthe[celluleCourante->ligne + 1][celluleCourante->colonne].visite) {
            mursPossibles[nombreMursPossibles++] = SUD;
        }

        // Ouest
        if (celluleCourante->colonne - 1 >= 0 && !labyrinthe[celluleCourante->ligne][celluleCourante->colonne - 1].visite) {
            mursPossibles[nombreMursPossibles++] = OUEST;
        }

        // Mur aléatoire à supprimer
        if (nombreMursPossibles > 0) {
            int murAleatoire = mursPossibles[rand() % nombreMursPossibles];

            // Supprimer le mur
            switch (murAleatoire) {
                case NORD:
                    celluleCourante->murs &= ~NORD;
                    labyrinthe[celluleCourante->ligne - 1][celluleCourante->colonne].murs &= ~SUD;
                    break;
                case EST:
                    celluleCourante->murs &= ~EST;
                    labyrinthe[celluleCourante->ligne][celluleCourante->colonne + 1].murs &= ~OUEST;
                    break;
                case SUD:
                    celluleCourante->murs &= ~SUD;
                    labyrinthe[celluleCourante->ligne + 1][celluleCourante->colonne].murs &= ~NORD;
                    break;
                case OUEST:
                    celluleCourante->murs &= ~OUEST;
                    labyrinthe[celluleCourante->ligne][celluleCourante->colonne - 1].murs &= ~EST;
                    break;
            }

            // Empiler la cellule voisine
            switch (murAleatoire) {
                case NORD:
                    pile[sommetPile++] = &labyrinthe[celluleCourante->ligne - 1][celluleCourante->colonne];
                    break;
                case EST:
                    pile[sommetPile++] = &labyrinthe[celluleCourante->ligne][celluleCourante->colonne + 1];
                    break;
                case SUD:
                    pile[sommetPile++] = &labyrinthe[celluleCourante->ligne + 1][celluleCourante->colonne];
                    break;
                case OUEST:
                    pile[sommetPile++] = &labyrinthe[celluleCourante->ligne][celluleCourante->colonne - 1];
                    break;
            }
        }
    }
}

// Fonction pour afficher le labyrinthe
void afficherLabyrinthe() {
    // Afficher les murs du haut
    printf("+");
    for (int i = 0; i < TAILLE; i++) {
        printf("---+");
    }
    printf("\n");

    // Afficher les murs gauche/droit et les cellules
    for (int i = 0; i < TAILLE; i++) {
        for (int j = 0; j < TAILLE; j++) {
            printf("| ");
            if (labyrinthe[i][j].murs & NORD) {
                printf("   ");
            } else {
                printf(" ");
            }
            if (labyrinthe[i][j].murs & EST) {
                printf("|");
            } else {
                printf(" ");
            }
        }
        printf("\n");

        // Afficher les murs du bas
        printf("+");
        for (int j = 0; j < TAILLE; j++) {
            if (labyrinthe[i][j].murs & SUD) {
                printf("---+");
            } else {
                printf("   +");
            }
        }
        printf("\n");
    }
}

// Fonction pour résoudre le labyrinthe
bool résoudreLabyrinthe(int ligneDépart, int colonneDépart, int *solution) {
    // Empiler la cellule de départ
    Cellule *pile[TAILLE * TAILLE];
    int sommetPile = 0;
    pile[sommetPile++] = &labyrinthe[ligneDépart][colonneDépart];

    // Marquer la cellule de départ comme visitée
    labyrinthe[ligneDépart][colonneDépart].visite = true;

    // Tant qu'il y a des cellules à visiter
    while (sommetPile > 0) {
        // Désempiler la cellule actuelle
        Cellule *celluleCourante = pile[--sommetPile];

        // Ajouter la cellule à la solution
        solution[sommetPile] = celluleCourante->ligne * TAILLE + celluleCourante->colonne;

        // Si c'est la cellule d'arrivée, retourner vrai
        if (celluleCourante->ligne == TAILLE - 1 && celluleCourante->colonne == TAILLE - 1) {
            return true;
        }

        // Liste des murs possibles à supprimer
        int mursPossibles[4];
        int nombreMursPossibles = 0;

        // Nord
        if (celluleCourante->ligne - 1 >= 0 && !labyrinthe[celluleCourante->ligne - 1][celluleCourante->colonne].visite && !(labyrinthe[celluleCourante->ligne][celluleCourante->colonne].murs & NORD)) {
            mursPossibles[nombreMursPossibles++] = NORD;
        }

        // Est
        if (celluleCourante->colonne + 1 < TAILLE && !labyrinthe[celluleCourante->ligne][celluleCourante->colonne + 1].visite && !(labyrinthe[celluleCourante->ligne][celluleCourante->colonne].murs & EST)) {
            mursPossibles[nombreMursPossibles++] = EST;
        }

        // Sud
        if (celluleCourante