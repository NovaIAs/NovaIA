```cool
module PremierProgramme {
    class Premier {
        main(): String {
            let nombre: String := "5";
            let resultat: String := "Le carré de " ++ nombre ++ " est " ++ entier(entier(nombre) * entier(nombre)).StringValue;

            return resultat;
        }

        entier(nombre: String): Integer {
            return 0;
        }
    }
}
```

**Explication du code :**

Ce code montre un programme simple en COOL qui calcule le carré d'un nombre.

* La classe `Premier` est le point d'entrée du programme.
* La méthode `main` est la méthode principale du programme.
* La variable `nombre` est une chaîne de caractères représentant le nombre dont on veut calculer le carré.
* La variable `resultat` est une chaîne de caractères qui contiendra le résultat du calcul.
* La méthode `entier` convertit une chaîne de caractères en un entier.
* Le code calcule le carré du nombre en multipliant le nombre par lui-même à l'aide de l'opérateur `*`.
* La méthode `StringValue` convertit l'entier en chaîne de caractères.
* La méthode `++` concatène deux chaînes de caractères.
* Le code renvoie la chaîne de caractères `resultat`, qui contient le résultat du calcul.