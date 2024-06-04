**Programme en COOL**

```cool
class Main {
    main() : Object {
        let sum : Int <- 0;
        let i : Int ;
        for i <- 1 to 100 loop {
            sum <- sum + i;
        };
        return i;
    }
}
```

**Explication**

Ce code COOL définit une classe `Main` contenant une méthode `main` qui calcule la somme des nombres de 1 à 100.

* **Classe `Main`:** La classe `Main` est la classe principale du programme. Elle contient une seule méthode, `main`.
* **Méthode `main`:** La méthode `main` est le point d'entrée du programme. Elle:
    * Initialise une variable `sum` de type `Int` à 0.
    * Initialise une variable `i` de type `Int`.
    * Utilise une boucle `for` pour incrémenter `i` de 1 à 100.
    * À chaque itération de la boucle, ajoute `i` à `sum`.
    * Retourne `i`, qui contient la valeur finale de 100.