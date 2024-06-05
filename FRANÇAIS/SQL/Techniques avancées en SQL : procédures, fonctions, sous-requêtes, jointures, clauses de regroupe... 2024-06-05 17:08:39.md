**Création d'une procédure stockée**

```sql
-- Création d'une procédure stockée nommée "ObtenirClient"
CREATE PROCEDURE ObtenirClient (
    @ClientId INT -- Paramètre d'entrée pour l'ID du client
)
AS
BEGIN
    -- Corps de la procédure stockée
    -- Instructions SQL ici
END
```

**Création d'une fonction en ligne**

```sql
-- Création d'une fonction en ligne nommée "CalculerRemise"
SELECT CalculerRemise(100, 0.1) -- Appeler la fonction en ligne avec les arguments
FROM DUAL;

CREATE FUNCTION CalculerRemise (
    @Montant DECIMAL(18, 2),
    @TauxRemise DECIMAL(4, 2)
)
RETURNS DECIMAL(18, 2)
AS
BEGIN
    -- Corps de la fonction en ligne
    -- Instructions SQL ici
END
```

**Utilisation de sous-requêtes**

```sql
-- Requête utilisant une sous-requête pour obtenir les détails du client avec la commande la plus récente
SELECT *
FROM Clients
WHERE IdClient IN (
    SELECT IdClient
    FROM Commandes
    WHERE DateCommande = (
        SELECT MAX(DateCommande)
        FROM Commandes
    )
);
```

**Utilisation de jointures**

```sql
-- Requête utilisant une jointure pour obtenir les détails du client et de la commande
SELECT *
FROM Clients c
JOIN Commandes o ON c.IdClient = o.IdClient
WHERE o.DateCommande BETWEEN '2023-01-01' AND '2023-12-31';
```

**Utilisation de clauses GROUP BY et HAVING**

```sql
-- Requête utilisant les clauses GROUP BY et HAVING pour obtenir le nombre de commandes par client et filtrer les clients avec plus de 3 commandes
SELECT c.NomClient, COUNT(*) AS NombreCommandes
FROM Clients c
JOIN Commandes o ON c.IdClient = o.IdClient
GROUP BY c.NomClient
HAVING COUNT(*) > 3;
```

**Utilisation de fonctions Windows**

```sql
-- Requête utilisant la fonction Windows RANK() pour classer les clients en fonction du montant total des commandes
SELECT c.NomClient, RANK() OVER (ORDER BY SUM(o.Montant)) AS Rang
FROM Clients c
JOIN Commandes o ON c.IdClient = o.IdClient
GROUP BY c.NomClient;
```