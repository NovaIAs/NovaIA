```sql
WITH tmp AS (
  SELECT
    *,
    ROW_NUMBER() OVER(PARTITION BY group_id ORDER BY create_date DESC) AS rn
  FROM orders
),
filtered AS (
  SELECT
    *
  FROM tmp
  WHERE rn = 1
),
grouped AS (
  SELECT
    group_id,
    GROUP_CONCAT(product_id || ' ' || quantity) AS products
  FROM filtered
  GROUP BY group_id
)
SELECT
  t1.order_date,
  t1.group_id,
  t1.customer_name,
  t2.products
FROM orders t1
JOIN grouped t2 ON t1.group_id = t2.group_id
WHERE t1.order_total > 100;
```

Explanation:

1. The tmp CTE adds a row number (rn) to each order based on its group_id and create_date. This helps identify the latest order for each group.

2. The filtered CTE filters the tmp CTE and keeps only the rows with rn = 1, which represent the latest orders for each group.

3. The grouped CTE groups the filtered CTE by group_id and concatenates the product_id and quantity for each order into a single string. This gives us a list of products with their quantities for each group.

4. The final SELECT statement joins the orders table (aliased as t1) with the grouped CTE (aliased as t2) based on group_id. It then filters the results to include only orders where the order_total is greater than 100.

This complex SQL code retrieves the latest orders for each group with a total amount greater than 100. It provides the order date, group ID, customer name, and a concatenated list of products and their quantities for each group.