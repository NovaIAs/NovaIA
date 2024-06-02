**Requête SQL complexe à plusieurs niveaux avec jointures, fonctions d'agrégation et sous-requêtes**

```sql
WITH Données_Commande AS (
  SELECT
    CommandeID,
    ClientID,
    DateCommande,
    MontantTotalCommande
  FROM Commandes
),
Produits_Commandés AS (
  SELECT
    CommandeID,
    ProductID,
    NomProduit,
    PrixUnitaire,
    QuantitéCommandée
  FROM Produits_Commandés
)
SELECT
  DC.CommandeID,
  C.NomClient,
  DC.DateCommande,
  DC.MontantTotalCommande,
  (
    SELECT
      SUM(PC.QuantitéCommandée)
    FROM Produits_Commandés AS PC
    WHERE
      PC.CommandeID = DC.CommandeID
    GROUP BY
      PC.CommandeID
  ) AS NombreTotalProduits,
  (
    SELECT
      SUM(PC.PrixUnitaire * PC.QuantitéCommandée)
    FROM Produits_Commandés AS PC
    WHERE
      PC.CommandeID = DC.CommandeID
    GROUP BY
      PC.CommandeID
  ) AS MontantTotalProduits,
  (
    SELECT
      AVG(PC.PrixUnitaire)
    FROM Produits_Commandés AS PC
    WHERE
      PC.CommandeID = DC.CommandeID
    GROUP BY
      PC.CommandeID
  ) AS PrixMoyenProduit
FROM Données_Commande AS DC
JOIN Clients AS C
  ON DC.ClientID = C.ClientID;
```

**Explication du code :**

Cette requête SQL complexe à plusieurs niveaux utilise des tables temporaires (vues) pour simplifier la structure de la requête et améliorer la lisibilité.

* **Données_Commande** : Vue qui sélectionne les données essentielles des commandes, notamment l'ID de la commande, l'ID du client, la date de la commande et le montant total de la commande.
* **Produits_Commandés** : Vue qui sélectionne les données des produits commandés, notamment l'ID de la commande, l'ID du produit, le nom du produit, le prix unitaire et la quantité commandée.

La requête principale utilise ces vues pour effectuer les opérations suivantes :

* Joint les tables **Données_Commande** et **Clients** sur l'ID du client pour récupérer les noms des clients.
* Calcule le nombre total de produits commandés pour chaque commande à l'aide d'une sous-requête dans la colonne **NombreTotalProduits**.
* Calcule le montant total des produits commandés pour chaque commande à l'aide d'une sous-requête dans la colonne **MontantTotalProduits**.
* Calcule le prix moyen des produits commandés pour chaque commande à l'aide d'une sous-requête dans la colonne **PrixMoyenProduit**.

Cette requête complexe permet d'obtenir une vue complète des commandes, y compris les informations sur les clients, les produits commandés et les statistiques sur les commandes.