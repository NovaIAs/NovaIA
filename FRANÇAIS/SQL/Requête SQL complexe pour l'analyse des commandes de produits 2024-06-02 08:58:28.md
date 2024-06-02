**Requête SQL complexe**

```sql
SELECT
  t1.Nom_produit,
  t2.Date_de_commande,
  t2.ID_commande,
  GROUP_CONCAT(DISTINCT t3.Nom_catégorie) AS Catégories_produit,
  MAX(t2.Quantité_commandée) AS Quantité_max_commandée
FROM Table_produits AS t1
JOIN Table_commandes AS t2
  ON t1.ID_produit = t2.ID_produit
JOIN Table_catégories_produits AS t3
  ON t1.ID_produit = t3.ID_produit
WHERE
  t2.Date_commande BETWEEN '2022-01-01' AND '2022-12-31'
GROUP BY
  t1.Nom_produit,
  t2.Date_de_commande,
  t2.ID_commande
ORDER BY
  t2.Date_de_commande DESC,
  Quantité_max_commandée DESC;
```

**Explication du code**

**SELECT** : Cette clause spécifie les colonnes à récupérer.

* `t1.Nom_produit` : Nom du produit.
* `t2.Date_de_commande` : Date de la commande.
* `t2.ID_commande` : Identifiant de la commande.
* `GROUP_CONCAT(DISTINCT t3.Nom_catégorie)` : Catégories des produits, concaténées et distinctes.
* `MAX(t2.Quantité_commandée)` : Quantité maximale commandée pour chaque produit.

**FROM** : Cette clause spécifie les tables impliquées dans la requête.

* `Table_produits (t1)` : Contient les informations sur les produits.
* `Table_commandes (t2)` : Contient les informations sur les commandes.
* `Table_catégories_produits (t3)` : Contient les catégories de produits.

**JOIN** : Ces clauses lient les tables sur la base de la clé étrangère `ID_produit`.

* `JOIN t2 ON t1.ID_produit = t2.ID_produit`
* `JOIN t3 ON t1.ID_produit = t3.ID_produit`

**WHERE** : Cette clause filtre les résultats pour n'inclure que les commandes passées entre le 1er janvier 2022 et le 31 décembre 2022.

* `t2.Date_commande BETWEEN '2022-01-01' AND '2022-12-31'`

**GROUP BY** : Cette clause regroupe les résultats par nom de produit, date de commande et identifiant de commande.

* `t1.Nom_produit, t2.Date_de_commande, t2.ID_commande`

**ORDER BY** : Cette clause trie les résultats par date de commande décroissante, puis par quantité maximale commandée décroissante.

* `t2.Date_de_commande DESC, Quantité_max_commandée DESC`

Cette requête complexe permet d'extraire des informations détaillées sur les produits, les commandes et les catégories, en fonction de critères de date spécifiques.