**Partie 1 : Définition de structures de données personnalisées**

```rust
// Structure représentant un graphe
struct Graphe {
    // Vecteur de listes d'arêtes pour chaque sommet
    arêtes: Vec<Vec<(usize, i32)>>,
}

// Structure représentant un arbre couvrant de Kruskal
struct ArbreKruskal {
    // Vecteur de parents pour chaque sommet
    parents: Vec<Option<usize>>,
    // Vecteur de poids pour chaque arête
    poids: Vec<i32>,
}
```

**Partie 2 : Implémentation de l'algorithme de Kruskal**

```rust
// Algorithme de Kruskal pour trouver un arbre couvrant de poids minimal
fn kruskal(graphe: &Graphe) -> Option<ArbreKruskal> {
    // Initialiser l'arbre couvrant
    let mut arbre = ArbreKruskal {
        parents: vec![None; graphe.arêtes.len()],
        poids: vec![],
    };

    // Trier les arêtes par poids
    let mut arêtes_triées = graphe.arêtes.iter().flatten().cloned().collect::<Vec<_>>();
    arêtes_triées.sort_by(|a, b| a.1.cmp(&b.1));

    // Parcourir les arêtes triées
    for (u, v, poids) in arêtes_triées {
        if union_find(&mut arbre.parents, u, v) {
            // Si les sommets ne sont pas déjà connectés, les connecter
            arbre.poids.push(poids);
        }
    }

    // Si le nombre d'arêtes dans l'arbre est égal au nombre de sommets - 1, l'arbre est un arbre couvrant
    if arbre.poids.len() == graphe.arêtes.len() - 1 {
        Some(arbre)
    } else {
        None
    }
}
```

**Partie 3 : Implémentation de l'algorithme Union-Find**

```rust
// Algorithme Union-Find pour maintenir des ensembles disjoints
fn union_find(parents: &mut Vec<Option<usize>>, u: usize, v: usize) -> bool {
    // Trouver les racines des ensembles contenant u et v
    let mut racine_u = find_parent(parents, u);
    let mut racine_v = find_parent(parents, v);

    // Si les racines sont différentes, fusionner les ensembles
    if racine_u != racine_v {
        parents[racine_u] = Some(racine_v);
        return true;
    }

    // Sinon, les sommets sont déjà connectés
    false
}

// Fonction auxiliaire pour trouver la racine d'un ensemble
fn find_parent(parents: &mut Vec<Option<usize>>, u: usize) -> usize {
    if let Some(parent) = parents[u] {
        find_parent(parents, parent)
    } else {
        u
    }
}
```

**Partie 4 : Exemple d'utilisation**

```rust
let graphe = Graphe {
    arêtes: vec![
        vec![(1, 3), (2, 4)],
        vec![(0, 3), (2, 5)],
        vec![(0, 4), (1, 5), (3, 6)],
        vec![(0, 3), (2, 6)],
    ],
};

if let Some(arbre_kruskal) = kruskal(&graphe) {
    println!("Poids total de l'arbre couvrant de poids minimal : {:?}", arbre_kruskal.poids.iter().sum::<i32>());
    for (u, parent) in arbre_kruskal.parents.iter().enumerate() {
        if let Some(parent) = parent {
            println!("Sommet {} est le parent de {}", u, parent);
        } else {
            println!("Sommet {} est la racine de son propre ensemble", u);
        }
    }
} else {
    println!("Aucun arbre couvrant de poids minimal trouvé");
}
```

**Explications :**

* Les structures de données `Graphe` et `ArbreKruskal` représentent respectivement le graphe d'entrée et l'arbre couvrant résultant.
* L'algorithme de Kruskal utilise l'algorithme Union-Find pour maintenir des ensembles disjoints, afin de garantir qu'aucun cycle n'est créé dans l'arbre couvrant.
* L'exemple d'utilisation crée un graphe, exécute l'algorithme de Kruskal et affiche le poids total et la structure de l'arbre couvrant de poids minimal trouvé.