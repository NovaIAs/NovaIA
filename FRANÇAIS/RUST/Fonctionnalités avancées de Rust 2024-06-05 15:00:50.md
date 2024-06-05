**Code complexe en Rust**

```rust
// **Module** : un espace de noms pour organiser le code.
mod outils {
    // **Fonction** : un bloc de code réutilisable qui prend des entrées et renvoie une valeur.
    pub fn maximum(a: i32, b: i32) -> i32 {
        if a > b {
            a
        } else {
            b
        }
    }
}

// **Type personnalisé** : une nouvelle structure de données définie par l'utilisateur.
#[derive(Debug)]
struct Point {
    x: i32,
    y: i32,
}

// **Fonction générique** : une fonction qui peut fonctionner avec des types différents.
fn somme<T: std::ops::Add<Output = T>>(a: T, b: T) -> T {
    a + b
}

// **Enumération** : un type qui représente un ensemble de valeurs distinctes.
enum Forme {
    Carre(i32),
    Rectangle(i32, i32),
    Cercle(f64),
}

// **Macro** : une règle qui permet d'écrire du code plus concis.
macro_rules! println {
    ($($arg:tt)*) => (std::println!($($arg)*));
}

// **Fonction avec des paramètres nommés** : une fonction qui prend des arguments avec des noms spécifiques.
fn affiche_coordonnées({ x, y }: Point) {
    println!("Coordonnées : ({}, {})", x, y);
}

// **Fermeture** : une fonction qui capture les variables de son environnement.
let somme = |x: i32, y: i32| -> i32 { x + y };

fn main() {
    // **Structure** : une instance d'un type personnalisé.
    let point = Point { x: 10, y: 20 };

    // **Appel de fonction du module** : utilisation de la fonction `maximum` du module `outils`.
    let max = outils::maximum(5, 10);

    // **Appel de fonction générique** : utilisation de la fonction `somme` avec différents types.
    let somme_entiers = somme(10, 20);
    let somme_flottants = somme(10.5, 20.7);

    // **Correspondance de modèle** : une expression qui compare une valeur à un motif.
    match point {
        Point { x, y } => println!("Point : ({}, {})", x, y),
        _ => println!("Type de point inconnu"),
    }

    // **Appel de macro** : utilisation de la macro `println!`.
    println!("Maximum : {}", max);
    println!("Somme des entiers : {}", somme_entiers);
    println!("Somme des flottants : {}", somme_flottants);

    // **Appel de fonction avec des paramètres nommés** : utilisation de la fonction `affiche_coordonnées`.
    affiche_coordonnées(point);

    // **Appel de fermeture** : utilisation de la fermeture `somme`.
    let resultat = somme(1, 2);
    println!("Résultat de la fermeture : {}", resultat);
}
```

**Explications**

* Le code est organisé en **modules** et **espaces de noms**, ce qui permet une meilleure organisation et lisibilité.
* Une **fonction** `maximum` est définie dans le module `outils`, qui prend deux arguments et renvoie la valeur maximale.
* Un **type personnalisé** `Point` est défini pour représenter les coordonnées d'un point.
* Une **fonction générique** `somme` est définie, qui peut additionner des valeurs de types différents.
* Une **énumération** `Forme` est définie pour représenter différentes formes géométriques.
* Une **macro** `println!` est définie pour simplifier l'appel de la fonction `println!`.
* Une **fonction avec des paramètres nommés** `affiche_coordonnées` est définie, qui prend un argument `Point` avec ses champs nommés.
* Une **fermeture** `somme` est définie, qui capture les variables de son environnement et peut être appelée n'importe où dans le programme.
* La fonction `main` est le point d'entrée du programme. Elle utilise les différentes fonctionnalités du code pour effectuer diverses opérations.