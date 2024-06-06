**Code SQL complexe**

```sql
/* Cette requête complexe combine plusieurs clauses et fonctions pour renvoyer un ensemble de résultats personnalisé à partir d'une base de données. */

WITH Données_analysées AS (
    -- Sous-requête pour calculer des statistiques de vente agrégées
    SELECT
        p.Nom_produit,
        SUM(v.Quantité) AS Quantité_vendue,
        AVG(v.Prix_unitaire) AS Prix_unitaire_moyen
    FROM
        Produits AS p
    JOIN
        Ventes AS v
        ON p.Id_produit = v.Id_produit
    GROUP BY
        p.Nom_produit
),

Ventes_par_client AS (
    -- Sous-requête pour calculer les ventes totales par client
    SELECT
        c.Nom_client,
        SUM(v.Quantité * v.Prix_unitaire) AS Ventes_totales
    FROM
        Clients AS c
    JOIN
        Ventes AS v
        ON c.Id_client = v.Id_client
    GROUP BY
        c.Nom_client
)

SELECT
    da.Nom_produit,
    da.Quantité_vendue,
    da.Prix_unitaire_moyen,
    vpc.Nom_client,
    vpc.Ventes_totales
FROM
    Données_analysées AS da
LEFT JOIN
    Ventes_par_client AS vpc
    ON da.Nom_produit = vpc.Nom_client
WHERE
    da.Quantité_vendue > 50
    AND da.Prix_unitaire_moyen > 10
ORDER BY
    da.Quantité_vendue DESC,
    da.Prix_unitaire_moyen DESC;
```

**Explication du code :**

**Clause WITH**

* Crée des sous-requêtes nommées (`Données_analysées` et `Ventes_par_client`) afin de réutiliser et d'optimiser le code.

**Sous-requête Données_analysées**

* Calcule les statistiques de vente agrégées (quantité vendue et prix unitaire moyen) pour chaque produit.

**Sous-requête Ventes_par_client**

* Calcule les ventes totales pour chaque client.

**Clause SELECT**

* Sélectionne les colonnes qui doivent être incluses dans le résultat final.

**Clause LEFT JOIN**

* Joint la sous-requête `Ventes_par_client` à la sous-requête `Données_analysées` sur la base du nom du produit. Cela permet d'obtenir des informations sur les clients qui ont acheté des produits spécifiques.

**Clause WHERE**

* Filtre les résultats pour n'inclure que les produits vendus plus de 50 fois et dont le prix unitaire moyen est supérieur à 10.

**Clause ORDER BY**

* Trie les résultats en ordre décroissant en fonction de la quantité vendue, puis du prix unitaire moyen.