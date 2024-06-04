**Code SQL complexe**

```sql
/* Requête imbriquée complexe pour récupérer les données de plusieurs tables et calculer des statistiques */

SELECT
    p1.nom AS nom_produit1,
    p2.nom AS nom_produit2,
    (
        SELECT SUM(v.quantite)
        FROM ventes AS v
        WHERE
            v.id_produit1 = p1.id_produit
            AND v.id_produit2 = p2.id_produit
    ) AS quantite_vendue,
    (
        SELECT SUM(v.montant)
        FROM ventes AS v
        WHERE
            v.id_produit1 = p1.id_produit
            AND v.id_produit2 = p2.id_produit
    ) AS montant_total
FROM
    produits AS p1
CROSS JOIN
    produits AS p2
WHERE
    p1.id_categorie = p2.id_categorie
    AND p1.id_produit < p2.id_produit
    AND EXISTS (
        SELECT 1
        FROM ventes AS v
        WHERE
            v.id_produit1 = p1.id_produit
            AND v.id_produit2 = p2.id_produit
    );
```

**Explication du code**

Cette requête SQL imbriquée exécute les tâches suivantes :

* **Jointure croisée de deux tables produits** : Cela combine chaque ligne de la table `produits` avec chaque ligne de la même table, mais avec une condition supplémentaire qui garantit que l'identifiant du produit dans la première table est inférieur à celui de la deuxième table.

* **Requête imbriquée pour calculer la quantité vendue** : La première sous-requête calcule la quantité totale vendue pour chaque paire de produits en additionnant les quantités de la table `ventes` où les identifiants des produits correspondent.

* **Requête imbriquée pour calculer le montant total** : La deuxième sous-requête calcule le montant total vendu pour chaque paire de produits en additionnant les montants de la table `ventes` où les identifiants des produits correspondent.

* **Filtre supplémentaire** : La clause `EXISTS` garantit qu'au moins une ligne existe dans la table `ventes` pour chaque paire de produits, en excluant les paires qui n'ont jamais été vendues ensemble.

Le résultat final est un tableau qui répertorie les noms des produits, la quantité vendue et le montant total pour toutes les paires de produits vendues au sein de la même catégorie.