```c#
// Générateur de labyrinthe de Prim (algorithme aléatoire)

using System;
using System.Collections.Generic;

// Représente une cellule du labyrinthe
public class Cellule
{
    public int X { get; set; }
    public int Y { get; set; }
    public bool Visitee { get; set; }
}

// Représente le labyrinthe
public class Labyrinthe
{
    public int Largeur { get; set; }
    public int Hauteur { get; set; }
    public Cellule[,] Grille { get; set; }

    // Constructeur
    public Labyrinthe(int largeur, int hauteur)
    {
        Largeur = largeur;
        Hauteur = hauteur;
        Grille = new Cellule[Largeur, Hauteur];
    }

    // Génère le labyrinthe aléatoire
    public void Generer()
    {
        // Liste des cellules non visitées
        List<Cellule> nonVisitees = new List<Cellule>();

        // Initialiser la grille et ajouter toutes les cellules à la liste des non visitées
        for (int x = 0; x < Largeur; x++)
        {
            for (int y = 0; y < Hauteur; y++)
            {
                Grille[x, y] = new Cellule { X = x, Y = y, Visitee = false };
                nonVisitees.Add(Grille[x, y]);
            }
        }

        // Choisir une cellule aléatoire comme point de départ
        Cellule depart = nonVisitees[Aleatoire(0, nonVisitees.Count - 1)];
        depart.Visitee = true;

        // Tant que des cellules non visitées existent
        while (nonVisitees.Count > 0)
        {
            // Liste des voisins non visités de la cellule courante
            List<Cellule> voisins = new List<Cellule>();

            // Trouver les voisins non visités
            Cellule courant = depart;
            if (courant.X > 0 && !Grille[courant.X - 1, courant.Y].Visitee)
                voisins.Add(Grille[courant.X - 1, courant.Y]);
            if (courant.X < Largeur - 1 && !Grille[courant.X + 1, courant.Y].Visitee)
                voisins.Add(Grille[courant.X + 1, courant.Y]);
            if (courant.Y > 0 && !Grille[courant.X, courant.Y - 1].Visitee)
                voisins.Add(Grille[courant.X, courant.Y - 1]);
            if (courant.Y < Hauteur - 1 && !Grille[courant.X, courant.Y + 1].Visitee)
                voisins.Add(Grille[courant.X, courant.Y + 1]);

            // Choisir un voisin aléatoire
            Cellule voisin = voisins[Aleatoire(0, voisins.Count - 1)];

            // Supprimer le mur entre la cellule courante et le voisin
            if (courant.X == voisin.X)
            {
                if (courant.Y == voisin.Y - 1)
                    voisin.Visitee = true;
                else
                    courant.Visitee = true;
            }
            else
            {
                if (courant.X == voisin.X - 1)
                    voisin.Visitee = true;
                else
                    courant.Visitee = true;
            }

            // Marquer le voisin comme visité et l'ajouter à la liste des non visitées
            voisin.Visitee = true;
            nonVisitees.Add(voisin);

            // La cellule courante devient le voisin
            depart = voisin;
        }
    }

    // Génère un nombre aléatoire entre min et max (inclus)
    private int Aleatoire(int min, int max)
    {
        return new Random().Next(min, max + 1);
    }
}
```

**Explications du code:**

* **Cellule:** Représente une cellule individuelle du labyrinthe avec ses coordonnées (X, Y) et son état de visite.
* **Labyrinthe:** Représente l'ensemble du labyrinthe en tant que tableau bidimensionnel de cellules.
* **Generer()**: Implémente l'algorithme de Prim pour générer un labyrinthe aléatoire. Voici les étapes:
    * Initialisation: Toutes les cellules sont non visitées et ajoutées à une liste.
    * Point de départ: Une cellule aléatoire est choisie comme point de départ et marquée comme visitée.
    * Itération: Tant que des cellules non visitées restent, on répète:
        * Trouver les voisins non visités de la cellule courante.
        * Choisir un voisin aléatoire et supprimer le mur entre eux.
        * Marquer le voisin comme visité et l'ajouter à la liste des non visitées.
        * Faire du voisin la cellule courante.
* **Aleatoire()**: Génère un nombre aléatoire entre une plage spécifiée.