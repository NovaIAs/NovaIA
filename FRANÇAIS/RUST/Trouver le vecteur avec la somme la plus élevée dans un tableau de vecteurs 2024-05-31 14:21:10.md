```rust
fn main() {
    // Créer un tableau de vecteurs
    let vecteurs = vec![
        vec![1, 2, 3],
        vec![4, 5, 6],
        vec![7, 8, 9],
    ];

    // Transformer le tableau de vecteurs en un vecteur de vecteurs
    let vecteur_de_vecteurs = vecteurs.into_iter().collect::<Vec<Vec<i32>>>();

    // Calculer la somme de chaque vecteur
    let sommes = vecteur_de_vecteurs.iter().map(|vecteur| vecteur.iter().sum()).collect::<Vec<i32>>();

    // Trouver le vecteur avec la somme la plus élevée
    let max_somme = sommes.iter().max().unwrap();

    // Afficher le vecteur avec la somme la plus élevée
    println!("Le vecteur avec la somme la plus élevée est : {:?}", vecteur_de_vecteurs[sommes.iter().position(|&x| x == max_somme).unwrap()]);
}
```

**Explication du code :**

* La fonction `main` est le point d'entrée du programme.

* Nous créons un tableau de vecteurs, où chaque vecteur contient une liste de nombres.

* Nous transformons le tableau de vecteurs en un vecteur de vecteurs à l'aide de la méthode `into_iter()`. Cela nous permet d'itérer sur chaque vecteur individuellement.

* Nous calculons la somme de chaque vecteur à l'aide de la méthode `iter()` et de la fonction `sum()`.

* Nous collectons les sommes dans un vecteur appelé `sommes`.

* Nous trouvons le vecteur avec la somme la plus élevée à l'aide de la méthode `max()`.

* Nous affichons le vecteur avec la somme la plus élevée à l'aide de la fonction `println!`.