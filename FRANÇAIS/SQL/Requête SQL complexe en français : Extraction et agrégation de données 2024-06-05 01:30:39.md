**Requête SQL complexe en français**

**Objectif :** Extrait des données d'une base de données complexe et les agrège en fonction de critères multiples

**Syntaxe :**

```sql
SELECT
    t1.nom_client,
    t1.adresse_client,
    t2.nom_produit,
    t2.quantite_vendue,
    SUM(t2.prix_unitaire * t2.quantite_vendue) AS montant_total
FROM
    clients t1
JOIN
    ventes t2 ON t1.id_client = t2.id_client
WHERE
    t1.ville_client = 'Paris'
    AND t2.date_vente BETWEEN '2023-01-01' AND '2023-12-31'
GROUP BY
    t1.nom_client,
    t1.adresse_client,
    t2.nom_produit
HAVING
    SUM(t2.prix_unitaire * t2.quantite_vendue) > 1000
ORDER BY
    montant_total DESC
LIMIT
    10;
```

**Explication :**

* La requête utilise les clauses `SELECT`, `FROM`, `JOIN`, `WHERE`, `GROUP BY`, `HAVING` et `ORDER BY` pour extraire, filtrer, agréger et trier les données de deux tables, `clients` et `ventes`.
* La clause `SELECT` sélectionne les colonnes souhaitées des tables : `nom_client`, `adresse_client`, `nom_produit`, `quantite_vendue` et `montant_total` (calculé par la fonction `SUM`).
* La clause `FROM` spécifie les tables jointes : `clients` (aliaisée en `t1`) et `ventes` (aliaisée en `t2`).
* La clause `JOIN` joint les tables sur la colonne `id_client`.
* La clause `WHERE` filtre les enregistrements par les critères : la ville du client est "Paris" et la date de vente est comprise entre le 1er janvier 2023 et le 31 décembre 2023.
* La clause `GROUP BY` regroupe les enregistrements par les colonnes `nom_client`, `adresse_client` et `nom_produit` pour effectuer le calcul des agrégats.
* La clause `HAVING` filtre les enregistrements groupés par ceux dont le montant total est supérieur à 1000.
* La clause `ORDER BY` trie les enregistrements par montant total décroissant.
* La clause `LIMIT` limite le nombre d'enregistrements renvoyés à 10.