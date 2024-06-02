**Bonjour le monde en Objective-C**

```objective-c
#import <Foundation/Foundation.h>

int main(int argc, const char *argv[]) {
    @autoreleasepool {

        // Crée un objet NSString pour stocker un message
        NSString *message = @"Bonjour, monde !";

        // Imprime le message dans la console
        NSLog(@"%@", message);
    }
    return 0;
}
```

**Explication :**

* La première ligne importe le framework Foundation, qui fournit les classes et fonctions de base pour les programmes Objective-C.
* La fonction `main()` est le point d'entrée du programme.
* `@autoreleasepool` crée un pool d'objets auto-libérés. Les objets créés dans ce bloc sont automatiquement libérés lorsque le bloc se termine.
* `NSString` est une classe qui représente une chaîne de caractères en Objective-C.
* `NSLog()` est une fonction qui imprime un message dans la console.

**Ajout d'arguments en ligne de commande**

```objective-c
#import <Foundation/Foundation.h>

int main(int argc, const char *argv[]) {
    @autoreleasepool {

        // Vérifie s'il y a au moins un argument
        if (argc > 1) {
            // Récupère le premier argument
            NSString *message = [NSString stringWithUTF8String:argv[1]];

            // Imprime le message dans la console
            NSLog(@"%@", message);
        } else {
            // Message par défaut si aucun argument n'est fourni
            NSLog(@"Bonjour, monde !");
        }
    }
    return 0;
}
```

**Explication :**

* La fonction `main()` est modifiée pour prendre des arguments en ligne de commande.
* `argc` est le nombre d'arguments et `argv` est un tableau de pointeurs vers les arguments.
* Si l'utilisateur a fourni un argument, il est utilisé comme message. Sinon, le message par défaut est affiché.

**Calcul de la moyenne d'une liste de nombres**

```objective-c
#import <Foundation/Foundation.h>

int main(int argc, const char *argv[]) {
    @autoreleasepool {

        // Crée un tableau de nombres
        NSArray *nombres = @[@1, @2, @3, @4, @5];

        // Calcule la somme des nombres
        NSNumber *somme = @0;
        for (NSNumber *nombre in nombres) {
            somme = @(somme.doubleValue + nombre.doubleValue);
        }

        // Calcule la moyenne
        double moyenne = somme.doubleValue / nombres.count;

        // Imprime la moyenne
        NSLog(@"Moyenne : %.2f", moyenne);
    }
    return 0;
}
```

**Explication :**

* Un tableau est créé pour stocker les nombres.
* La boucle `for` itère sur les nombres et calcule leur somme.
* La moyenne est calculée en divisant la somme par le nombre d'éléments.
* `NSLog()` est utilisé pour imprimer la moyenne avec deux décimales.

**Gestion d'exceptions avec @try et @catch**

```objective-c
#import <Foundation/Foundation.h>

int main(int argc, const char *argv[]) {
    @autoreleasepool {

        // Crée un tableau de nombres
        NSArray *nombres = @[@1, @2, @3, @4, @5];

        // Gère les exceptions
        @try {
            // Accède à un index inexistant
            NSNumber *nombre = nombres[nombres.count];
        } @catch (NSException *exception) {
            // Traite l'exception
            NSLog(@"Exception : %@", exception);
        } @finally {
            // Code exécuté toujours, même en cas d'exception
            NSLog(@"Fin du bloc @try/@catch/@finally");
        }
    }
    return 0;
}
```

**Explication :**

* `@try` entoure le code qui peut générer des exceptions.
* `@catch` capture les exceptions et permet de les traiter.
* `@finally` est exécuté toujours, quelle que soit l'issue du bloc `@try/@catch`.
* Dans cet exemple, une exception est générée lorsque l'index d'un élément inexistant est accédé.