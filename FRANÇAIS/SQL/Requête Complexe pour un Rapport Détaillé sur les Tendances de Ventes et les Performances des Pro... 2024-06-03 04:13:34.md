```sql
**Version de schéma de la base de données :** 1.5.0

**Requête complexe pour générer un rapport détaillé sur les tendances de vente et les performances des produits**

```sql
/*
Cette requête complexe est conçue pour générer un rapport détaillé sur les tendances de vente et les performances des produits, en fournissant des informations précieuses pour l'analyse commerciale et la prise de décision stratégique.
*/

/*
1. Sous-requête pour calculer les indicateurs de performance clés (KPI) pour chaque produit
*/
WITH ProductKPIs AS (
    SELECT
        p.ProductID,
        p.ProductName,
        AVG(s.UnitPrice) AS AverageUnitPrice,
        SUM(s.Quantity) AS TotalQuantitySold,
        SUM(s.UnitPrice * s.Quantity) AS TotalRevenue
    FROM
        Products p
    JOIN
        Sales s ON p.ProductID = s.ProductID
    GROUP BY
        p.ProductID, p.ProductName
)

/*
2. Sous-requête pour calculer les taux de croissance des ventes au fil du temps
*/
WITH SalesOverTime AS (
    SELECT
        ProductID,
        SalesDate,
        TotalRevenue,
        LAG(TotalRevenue, 1, 0) OVER (PARTITION BY ProductID ORDER BY SalesDate) AS PreviousRevenue
    FROM
        ProductKPIs
)

/*
3. Sous-requête pour calculer les écarts de performance par rapport aux objectifs
*/
WITH PerformanceGap AS (
    SELECT
        ProductID,
        SalesDate,
        TotalRevenue,
        SUM(TotalRevenue) OVER (PARTITION BY ProductID ORDER BY SalesDate) AS CumulativeTotalRevenue,
        (TotalRevenue - (CASE WHEN SalesDate <= TargetEndDate THEN TargetRevenue ELSE 0 END)) AS PerformanceGap
    FROM
        SalesOverTime
    JOIN
        SalesTargets t ON s.ProductID = t.ProductID
)

/*
4. Requete principale pour générer le rapport détaillé
*/
SELECT
    kp.ProductID,
    kp.ProductName,
    kp.AverageUnitPrice,
    kp.TotalQuantitySold,
    kp.TotalRevenue,
    pg.GrowthRate AS SalesGrowthRate,
    pgf.PerformanceGap AS PerformanceGap
FROM
    ProductKPIs kp
JOIN
    PerformanceGap pgf ON kp.ProductID = pgf.ProductID
JOIN
    SalesOverTime pg ON kp.ProductID = pg.ProductID
ORDER BY
    kp.TotalRevenue DESC;
```

**Explication du code :**

**1. Sous-requête ProductKPIs :**
- Calcule les indicateurs de performance clés (KPI) pour chaque produit, notamment le prix unitaire moyen, la quantité totale vendue et le revenu total.

**2. Sous-requête SalesOverTime :**
- Calcule les taux de croissance des ventes au fil du temps en calculant la différence entre les revenus totaux actuels et précédents.

**3. Sous-requête PerformanceGap :**
- Calcule les écarts de performance par rapport aux objectifs en comparant les revenus totaux avec les revenus cibles.

**4. Requête principale :**
- Joint les résultats des sous-requêtes pour générer le rapport détaillé, qui comprend :
    - Informations sur les produits (ID, nom, prix unitaire moyen, quantité vendue, revenus totaux)
    - Taux de croissance des ventes
    - Écarts de performance par rapport aux objectifs