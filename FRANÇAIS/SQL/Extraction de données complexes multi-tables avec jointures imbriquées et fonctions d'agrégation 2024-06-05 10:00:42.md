**Requête pour extraire des données complexes à partir de plusieurs tables avec des conditions de jointure imbriquées et des fonctions d'agrégation**

```sql
SELECT
  t1.id_client,
  t2.nom_produit,
  SUM(t3.quantite) AS quantite_totale,
  AVG(t3.prix_unitaire) AS prix_moyen
-- Des tables t1, t2 et t3
FROM table1 AS t1
JOIN table2 AS t2
  ON t1.id_client = t2.id_client
JOIN table3 AS t3
  ON t2.id_produit = t3.id_produit
WHERE
  t3.quantite > 10
  AND EXISTS(
    SELECT
      1
    FROM table4
    WHERE
      table4.id_client = t1.id_client AND table4.statut = 'actif'
  )
GROUP BY
  t1.id_client,
  t2.nom_produit
ORDER BY
  quantite_totale DESC,
  prix_moyen ASC;
```

**Explication du code :**

* **Jointures imbriquées :**
  * La requête joint trois tables (t1, t2 et t3) à l'aide de jointures internes.
  * La jointure entre t2 et t3 utilise la colonne id_produit pour établir une relation entre les produits et les commandes.
* **Condition WHERE avec sous-requête :**
  * La clause WHERE vérifie que la quantité commandée (quantite dans t3) est supérieure à 10.
  * Elle utilise également une sous-requête EXISTS pour vérifier si le client (identifié par id_client dans t1) a un statut actif dans une autre table (table4). Cela garantit que seuls les clients actifs sont inclus dans les résultats.
* **Fonctions d'agrégation :**
  * SUM(t3.quantite) calcule la quantité totale de produits commandés par client.
  * AVG(t3.prix_unitaire) calcule le prix moyen des produits commandés par client.
* **Clause GROUP BY :**
  * La clause GROUP BY agrège les résultats par id_client et nom_produit, ce qui permet de calculer les quantités et les prix moyens pour chaque client et chaque produit.
* **Clause ORDER BY :**
  * La clause ORDER BY trie les résultats en ordre décroissant de quantite_totale et en ordre croissant de prix_moyen. Cela affiche les clients avec les commandes les plus importantes en premier et les produits avec les prix moyens les plus bas en premier.