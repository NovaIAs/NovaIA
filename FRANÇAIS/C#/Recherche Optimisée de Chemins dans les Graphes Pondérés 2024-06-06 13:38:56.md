**Classe de Recherche de Chemin Optimisée**

**Objectif:** Trouver le chemin le plus court entre deux points dans un graphe pondéré, en utilisant une approche de programmation dynamique.

**Code:**

```c#
using System;
using System.Collections.Generic;

public class RechercheCheminOptimisee
{
    // Matrice des poids du graphe
    private int[,] poids;

    // Matrice des indices des chemins les plus courts
    private int[,] indicesChemins;

    // Tableau des distances des plus courts chemins
    private int[] distances;

    // Ensemble des sommets non visités
    private HashSet<int> sommetsNonVisites;

    // Constructeur
    public RechercheCheminOptimisee(int[,] poids)
    {
        this.poids = poids;
        this.distances = new int[poids.GetLength(0)];
        this.indicesChemins = new int[poids.GetLength(0), poids.GetLength(1)];
        this.sommetsNonVisites = new HashSet<int>();

        for (int i = 0; i < poids.GetLength(0); i++)
        {
            distances[i] = int.MaxValue;
            sommetsNonVisites.Add(i);
        }
    }

    // Méthode de recherche du chemin le plus court
    public int RechercheChemin(int depart, int arrivee)
    {
        // Initialisation
        distances[depart] = 0;

        // Tant qu'il reste des sommets non visités
        while (sommetsNonVisites.Count > 0)
        {
            // Choix du sommet non visité ayant la plus courte distance
            int sommetCourant = -1;
            int distanceCourante = int.MaxValue;
            foreach (int sommet in sommetsNonVisites)
            {
                if (distances[sommet] < distanceCourante)
                {
                    sommetCourant = sommet;
                    distanceCourante = distances[sommet];
                }
            }

            // Suppression du sommet courant de l'ensemble des sommets non visités
            sommetsNonVisites.Remove(sommetCourant);

            // Pour chaque voisin du sommet courant
            for (int voisin = 0; voisin < poids.GetLength(1); voisin++)
            {
                // Si le poids de l'arête entre les deux sommets est positif
                if (poids[sommetCourant, voisin] > 0)
                {
                    // Calcul de la nouvelle distance
                    int nouvelleDistance = distances[sommetCourant] + poids[sommetCourant, voisin];

                    // Si la nouvelle distance est plus courte que la distance actuelle
                    if (nouvelleDistance < distances[voisin])
                    {
                        // Mise à jour de la distance
                        distances[voisin] = nouvelleDistance;

                        // Mise à jour de l'indice du chemin le plus court
                        indicesChemins[voisin, arrivee] = sommetCourant;
                    }
                }
            }
        }

        // Retour de la distance du chemin le plus court
        return distances[arrivee];
    }

    // Méthode de reconstruction du chemin le plus court
    public List<int> ReconstruireChemin(int depart, int arrivee)
    {
        // Liste des sommets du chemin le plus court
        List<int> chemin = new List<int>();

        // Initialisation avec le sommet d'arrivée
        chemin.Add(arrivee);

        // Recherche du sommet précédent jusqu'au sommet de départ
        int sommetCourant = indicesChemins[arrivee, depart];
        while (sommetCourant != depart)
        {
            chemin.Add(sommetCourant);
            sommetCourant = indicesChemins[sommetCourant, depart];
        }

        // Ajout du sommet de départ au chemin
        chemin.Add(depart);

        // Inversion de la liste pour obtenir l'ordre correct du chemin
        chemin.Reverse();

        // Retour du chemin le plus court
        return chemin;
    }
}
```

**Explication:**

Le code implémente l'algorithme de Dijkstra pour trouver le chemin le plus court entre deux sommets dans un graphe pondéré.

* **Classe:** La classe `RechercheCheminOptimisee` représente la logique de recherche de chemin.
* **Matrice des Poids:** La matrice `poids` stocke les poids des arêtes du graphe.
* **Matrice des Indices des Chemins les Plus Courts:** La matrice `indicesChemins` stocke les indices des sommets précédents sur le chemin le plus court vers chaque sommet.
* **Tableau des Distances:** Le tableau `distances` stocke les distances des plus courts chemins vers chaque sommet.
* **Ensemble des Sommets Non Visités:** L'ensemble `sommetsNonVisites` contient les sommets qui n'ont pas encore été visités par l'algorithme.

**Méthode de Recherche du Chemin:**

La méthode `RechercheChemin` implémente l'algorithme de Dijkstra pour trouver le chemin le plus court entre deux sommets. Elle se termine lorsque tous les sommets ont été visités ou qu'aucun chemin plus court ne peut être trouvé.

**Méthode de Reconstruction du Chemin:**

La méthode `ReconstruireChemin` utilise la matrice `indicesChemins` pour reconstruire le chemin le plus court du sommet de départ au sommet d'arrivée.

**Utilisation:**

Pour utiliser la classe, créez une instance avec la matrice des poids du graphe. Utilisez ensuite la méthode `RechercheChemin` pour trouver la distance du chemin le plus court entre deux sommets. Enfin, utilisez la méthode `ReconstruireChemin` pour obtenir la liste des sommets sur le chemin le plus court.