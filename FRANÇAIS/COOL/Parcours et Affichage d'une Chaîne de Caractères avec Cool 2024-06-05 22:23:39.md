```cool

class Main {
    main() : Object {
        let data : String <- "Ceci est un test";
        let size : Int <- data.size();
        let res : String <- "";
        for i : Int <- 0 to< size do
            res <- res + data[i];
        end;
        res <- res + "\n";
        res.write();
    }
}

```

**Explication du code :**

Ce code Cool crée une chaîne de caractères, récupère sa taille, parcourt la chaîne, et affiche la chaîne à l'écran. Voici une explication détaillée :

**1. Création d'une chaîne de caractères :**

```cool
let data : String <- "Ceci est un test";
```

Cette ligne crée une variable nommée `data` qui stocke la chaîne de caractères `"Ceci est un test"`. Le type `String` représente les chaînes de caractères dans Cool.

**2. Récupération de la taille de la chaîne :**

```cool
let size : Int <- data.size();
```

Cette ligne crée une variable nommée `size` qui stocke la taille de la chaîne `data`. La méthode `size()` renvoie la longueur de la chaîne de caractères.

**3. Initialisation d'une chaîne de caractères vide :**

```cool
let res : String <- "";
```

Cette ligne crée une variable nommée `res` qui stocke une chaîne de caractères vide.

**4. Boucle pour parcourir la chaîne :**

```cool
for i : Int <- 0 to< size do
    res <- res + data[i];
end;
```

Cette boucle `for` parcourt chaque caractère de la chaîne `data`. Elle utilise la variable `i` pour représenter l'index du caractère actuel. À chaque itération, elle ajoute le caractère `data[i]` à la chaîne `res`.

**5. Ajout d'une nouvelle ligne à la chaîne :**

```cool
res <- res + "\n";
```

Cette ligne ajoute une nouvelle ligne à la chaîne `res`. Cela est fait pour assurer que la chaîne entière est affichée sur une nouvelle ligne lorsqu'elle est imprimée.

**6. Affichage de la chaîne :**

```cool
res.write();
```

Enfin, cette ligne affiche la chaîne `res` à l'écran. La méthode `write()` imprime la chaîne sur la sortie standard.