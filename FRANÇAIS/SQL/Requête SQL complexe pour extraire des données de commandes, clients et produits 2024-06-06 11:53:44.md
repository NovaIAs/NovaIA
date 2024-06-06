**Requête SQL complexe en français**

```sql
/* Cette requête récupère des données sur les commandes, les clients et les produits dans une base de données de commerce électronique. */

SELECT
  Commandes.IdCommande,
  Commandes.DateCommande,
  Commandes.MontantTotal,
  Clients.NomClient,
  Produits.NomProduit,
  Produits.PrixUnitaire,
  LignesCommandes.Quantité

FROM Commandes

INNER JOIN Clients
  ON Commandes.IdClient = Clients.IdClient

INNER JOIN LignesCommandes
  ON Commandes.IdCommande = LignesCommandes.IdCommande

INNER JOIN Produits
  ON LignesCommandes.IdProduit = Produits.IdProduit

WHERE
  Commandes.DateCommande BETWEEN '2023-01-01' AND '2023-12-31'
  AND Clients.NomClient LIKE '%Dupont%'
  AND NOT Produits.NomProduit LIKE '%Ordinateur portable%'
  AND LignesCommandes.Quantité > 5

GROUP BY
  Commandes.IdCommande

HAVING
  COUNT(DISTINCT Produits.NomProduit) > 2

ORDER BY
  Commandes.MontantTotal DESC;
```

**Explication de la requête :**

* **Sélection des colonnes :** La requête sélectionne des colonnes de quatre tables différentes : `Commandes`, `Clients`, `Produits` et `LignesCommandes`. Cela permet de récupérer des informations sur les commandes, les clients, les produits et les lignes de commande.
* **Jointures :** La requête utilise des jointures `INNER JOIN` pour relier les tables `Commandes`, `Clients`, `LignesCommandes` et `Produits`. Cela garantit que seules les lignes qui correspondent dans toutes les tables sont renvoyées.
* **Clause `WHERE` :** La clause `WHERE` filtre les résultats pour inclure uniquement les commandes passées entre le 1er janvier et le 31 décembre 2023, les clients dont le nom contient "Dupont", les produits qui ne sont pas des "ordinateurs portables" et les lignes de commande avec une quantité supérieure à 5.
* **Clause `GROUP BY` :** La clause `GROUP BY` regroupe les résultats par `IdCommande`. Cela permet de calculer les statistiques pour chaque commande.
* **Clause `HAVING` :** La clause `HAVING` filtre les résultats groupés pour inclure uniquement les commandes comportant au moins 3 produits différents.
* **Clause `ORDER BY` :** La clause `ORDER BY` trie les résultats dans l'ordre décroissant du `MontantTotal`.