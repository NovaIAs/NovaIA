**Code complexe en COOL**

```cool
class Main {
    main(): String {
        let total := 0;
        let count := 0;
        while count < 100 do {
            let randomNumber := (random.nextInt() % 100) + 1;
            let divisibleByFive := randomNumber % 5 == 0;
            if divisibleByFive then {
                total := total + randomNumber;
            }
            count := count + 1;
        };
        return "La somme des nombres divisibles par 5 entre 1 et 100 est : " + total.toString();
    }
}
```

**Explication du code**

Ce code implémente un algorithme pour calculer la somme des nombres divisibles par 5 entre 1 et 100.

* La classe `Main` définit une fonction `main` qui est l'entrée du programme.
* La variable `total` est initialisée à 0 pour stocker la somme des nombres divisibles par 5.
* La variable `count` est initialisée à 0 pour compter le nombre d'itérations.
* La boucle `while` s'exécute tant que `count` est inférieur à 100.
* À chaque itération, un nombre aléatoire est généré et stocké dans la variable `randomNumber`.
* La variable `divisibleByFive` est définie à `true` si `randomNumber` est divisible par 5, sinon `false`.
* Si `randomNumber` est divisible par 5, il est ajouté à `total`.
* `count` est incrémenté de 1 à chaque itération.
* La fonction `toString` est utilisée pour convertir `total` en une chaîne.
* Le message final est retourné, indiquant la somme des nombres divisibles par 5 entre 1 et 100.