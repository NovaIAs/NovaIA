**Requête SQL complexe en français**

**Objectif :** Extraire et agréger des données à partir de plusieurs tables liées dans une base de données relationnelle.

**Syntaxe :**

```sql
SELECT
  p.produit_id,
  p.nom_produit,
  c.categorie_nom,
  c.description_categorie,
  SUM(v.quantite) AS quantite_vendue
FROM Produits AS p
JOIN Categories AS c
  ON p.categorie_id = c.categorie_id
JOIN Ventes AS v
  ON p.produit_id = v.produit_id
GROUP BY
  p.produit_id,
  p.nom_produit,
  c.categorie_nom,
  c.description_categorie
ORDER BY
  quantite_vendue DESC;
```

**Tables impliquées :**

* **Produits** : Contient les informations sur les produits.
* **Categories** : Contient les informations sur les catégories de produits.
* **Ventes** : Contient les informations sur les ventes des produits.

**Colonnes sélectionnées :**

* **produit_id** : Identifiant du produit.
* **nom_produit** : Nom du produit.
* **categorie_nom** : Nom de la catégorie du produit.
* **description_categorie** : Description de la catégorie du produit.
* **quantite_vendue** : Quantité totale vendue pour chaque produit.

**Jointures :**

* **JOIN** relie la table **Produits** à la table **Categories** sur la colonne **categorie_id**.
* **JOIN** relie la table **Produits** à la table **Ventes** sur la colonne **produit_id**.

**Clause GROUP BY :**

* Regroupe les résultats par les colonnes **produit_id**, **nom_produit**, **categorie_nom** et **description_categorie**.

**Clause ORDER BY :**

* Trie les résultats par la colonne **quantite_vendue** en ordre décroissant.

**Explication :**

La requête sélectionne des informations sur les produits, les catégories et les ventes. Elle utilise des jointures pour relier les tables sur des colonnes clés. La clause **GROUP BY** regroupe les résultats par produit et catégorie, et la clause **ORDER BY** trie les résultats par quantité vendue. Cela permet d'obtenir un rapport qui résume les performances de vente pour chaque produit dans chaque catégorie.