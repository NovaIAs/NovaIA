**Requête SQL complexe**

```sql
SELECT
    p.Nom_produit,
    p.Categorie_produit,
    SUM(v.Quantité_vendue) AS Quantité_totale_vendue,
    AVG(v.Prix_unitaire) AS Prix_moyen,
    MAX(v.Date_vente) AS Date_dernière_vente
FROM
    Produits p
JOIN
    Ventes v ON p.ID_produit = v.ID_produit
GROUP BY
    p.Nom_produit,
    p.Categorie_produit
HAVING
    Quantité_totale_vendue > 100
ORDER BY
    Quantité_totale_vendue DESC
LIMIT 10;
```

**Explication du code**

Cette requête SQL complexe effectue les opérations suivantes :

* **Jointure des tables Produits et Ventes** : La requête joint les tables Produits (p) et Ventes (v) en fonction de la colonne ID_produit commune, ce qui permet de relier les informations sur les produits aux informations sur les ventes.
* **Regroupement des données** : La requête regroupe les résultats par Nom_produit et Categorie_produit des produits, ce qui permet de calculer des statistiques agrégées pour chaque combinaison de produit et de catégorie.
* **Calcul des statistiques agrégées** : La requête calcule les statistiques agrégées suivantes pour chaque groupe :
    * Quantité_totale_vendue : La somme de la Quantité_vendue pour toutes les lignes de vente pour chaque combinaison produit/catégorie.
    * Prix_moyen : La moyenne du Prix_unitaire pour toutes les lignes de vente pour chaque combinaison produit/catégorie.
    * Date_dernière_vente : La date maximale de la Date_vente pour toutes les lignes de vente pour chaque combinaison produit/catégorie, ce qui indique la dernière date à laquelle un produit a été vendu.
* **Clause HAVING** : La clause HAVING filtre les résultats pour n'inclure que les groupes de produits et de catégories pour lesquels la Quantité_totale_vendue est supérieure à 100.
* **Tri des résultats** : La requête trie les résultats par Quantité_totale_vendue dans l'ordre décroissant, ce qui place les produits les plus vendus en premier.
* **Limite des résultats** : La clause LIMIT limite les résultats aux 10 premiers groupes de produits et de catégories, ce qui permet d'obtenir les informations sur les produits les plus populaires.