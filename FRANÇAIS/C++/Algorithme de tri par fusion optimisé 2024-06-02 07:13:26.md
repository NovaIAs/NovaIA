**Algorithme de tri complexe de fusion (en français)**

**Prototype de fonction:**

```cpp
vector<int> fusion(vector<int>& gauche, vector<int>& droite)
```

**Explication du code:**

```cpp
vector<int> fusion(vector<int>& gauche, vector<int>& droite)
{
    // Créer un vecteur pour stocker le résultat trié
    vector<int> resultat;

    // Initialiser les indices des vecteurs gauche et droite
    int indexGauche = 0;
    int indexDroite = 0;

    // Tant que les deux vecteurs ne sont pas vides
    while (indexGauche < gauche.size() && indexDroite < droite.size())
    {
        // Comparer les éléments aux indices actuels
        if (gauche[indexGauche] < droite[indexDroite])
        {
            // Ajouter l'élément du vecteur gauche au résultat
            resultat.push_back(gauche[indexGauche]);
            indexGauche++;
        }
        else
        {
            // Ajouter l'élément du vecteur droite au résultat
            resultat.push_back(droite[indexDroite]);
            indexDroite++;
        }
    }

    // Ajouter les éléments restants du vecteur gauche, s'il y en a
    while (indexGauche < gauche.size())
    {
        resultat.push_back(gauche[indexGauche]);
        indexGauche++;
    }

    // Ajouter les éléments restants du vecteur droite, s'il y en a
    while (indexDroite < droite.size())
    {
        resultat.push_back(droite[indexDroite]);
        indexDroite++;
    }

    // Renvoyer le vecteur trié
    return resultat;
}
```

**Explication détaillée:**

* La fonction `fusion` prend deux vecteurs d'entiers triés, `gauche` et `droite`, et les fusionne en un seul vecteur trié.
* La fonction utilise une approche de fusion itérative pour combiner les éléments des deux vecteurs.
* Elle compare les éléments aux indices actuels des deux vecteurs et ajoute le plus petit élément au vecteur résultat.
* Elle incrémente ensuite l'indice du vecteur qui a contribué l'élément le plus petit.
* Une fois que tous les éléments d'un vecteur ont été ajoutés au résultat, la fonction ajoute les éléments restants de l'autre vecteur.
* Enfin, la fonction renvoie le vecteur trié résultant.

**Complexité:**

La complexité temporelle de l'algorithme de tri par fusion est O(n log n), où n est le nombre total d'éléments dans les vecteurs d'entrée.