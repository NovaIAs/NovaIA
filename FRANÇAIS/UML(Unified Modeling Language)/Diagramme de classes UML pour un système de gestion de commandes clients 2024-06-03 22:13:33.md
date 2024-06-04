**Classe Client**

```uml
Client {
    - id: String
    - nom: String
    - prenom: String
    - adresse: Adresse
    - commandes: List<Commande>
}
```

**Classe Adresse**

```uml
Adresse {
    - id: String
    - numero: Int
    - rue: String
    - codePostal: Int
    - ville: String
    - pays: String
}
```

**Classe Commande**

```uml
Commande {
    - id: String
    - date: Date
    - montant: Double
    - client: Client
    - lignesCommande: List<LigneCommande>
}
```

**Classe LigneCommande**

```uml
LigneCommande {
    - id: String
    - quantite: Int
    - produit: Produit
    - commande: Commande
}
```

**Classe Produit**

```uml
Produit {
    - id: String
    - nom: String
    - description: String
    - prix: Double
    - categorie: Categorie
}
```

**Classe Categorie**

```uml
Categorie {
    - id: String
    - nom: String
    - produits: List<Produit>
}
```

**Relations**

- Client -> Adresse (1:1)
- Client -> Commande (1:n)
- Commande -> LigneCommande (1:n)
- LigneCommande -> Produit (1:1)
- Produit -> Categorie (1:n)

**Comportement**

- Le client peut passer des commandes.
- Les commandes contiennent des lignes de commande.
- Les lignes de commande représentent des produits.
- Les produits peuvent appartenir à des catégories.