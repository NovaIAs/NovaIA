**Requête complexe SQL pour analyser les données de vente et de produits**

**EXPLICATION**

Cette requête SQL complexe effectue une analyse approfondie des données de vente et de produits, fournissant des informations détaillées sur les ventes par produit, client et période.

**CODE SQL**

```sql
/* Extraire les données de vente et de produits */
SELECT
  p.product_id,
  p.product_name,
  c.customer_id,
  c.customer_name,
  s.sale_id,
  s.sale_date,
  s.quantity_sold,
  s.unit_price
FROM products AS p
JOIN sales AS s
  ON p.product_id = s.product_id
JOIN customers AS c
  ON s.customer_id = c.customer_id;

/* Regrouper les données par produit et client */
GROUP BY
  p.product_id,
  c.customer_id;

/* Calculer les quantités et les ventes totales vendues */
SELECT
  product_id,
  product_name,
  customer_id,
  customer_name,
  SUM(quantity_sold) AS total_quantity_sold,
  SUM(unit_price * quantity_sold) AS total_sales_value
FROM (
  SELECT
    p.product_id,
    p.product_name,
    c.customer_id,
    c.customer_name,
    s.sale_id,
    s.sale_date,
    s.quantity_sold,
    s.unit_price,
    ROW_NUMBER() OVER (PARTITION BY p.product_id, c.customer_id ORDER BY s.sale_date) AS row_num
  FROM products AS p
  JOIN sales AS s
    ON p.product_id = s.product_id
  JOIN customers AS c
    ON s.customer_id = c.customer_id
) AS t
GROUP BY
  product_id,
  customer_id;

/* Filtrer les résultats par période et quantité vendue */
HAVING
  sale_date BETWEEN '2023-01-01' AND '2023-12-31'
  AND total_quantity_sold > 10;

/* Trier les résultats par quantité vendue décroissante */
ORDER BY
  total_quantity_sold DESC;
```

**EXPLICATION DES CLAUSES**

* **SELECT:** Renvoie les colonnes souhaitées.
* **FROM:** Spécifie les tables à joindre.
* **JOIN:** Joint les tables sur la base de colonnes communes.
* **GROUP BY:** Regroupe les lignes par les colonnes spécifiées.
* **SUM:** Calcule les totaux des colonnes spécifiées.
* **HAVING:** Filtre les lignes en fonction de la condition spécifiée.
* **ORDER BY:** Trie les résultats dans l'ordre spécifié.