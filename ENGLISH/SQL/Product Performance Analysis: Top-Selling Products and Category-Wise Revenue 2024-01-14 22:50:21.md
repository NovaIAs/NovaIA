```sql
-- Create a temporary table to store intermediate results
CREATE TEMP TABLE IntermediateResults AS
SELECT
    p.product_id,
    p.product_name,
    p.product_category,
    p.product_price,
    s.store_name,
    s.store_location,
    o.order_id,
    o.order_date,
    o.order_quantity
FROM
    Products p
JOIN
    Stores s ON p.product_category = s.store_category
JOIN
    Orders o ON p.product_id = o.product_id;

-- Calculate the total revenue for each product in each store
CREATE TEMP TABLE ProductRevenue AS
SELECT
    product_id,
    product_name,
    product_category,
    store_name,
    store_location,
    SUM(order_quantity * product_price) AS total_revenue
FROM
    IntermediateResults
GROUP BY
    product_id,
    product_name,
    product_category,
    store_name,
    store_location;

-- Calculate the average revenue for each product category across all stores
CREATE TEMP TABLE CategoryRevenue AS
SELECT
    product_category,
    AVG(total_revenue) AS avg_category_revenue
FROM
    ProductRevenue
GROUP BY
    product_category;

-- Calculate the top 3 highest-revenue products in each store
CREATE TEMP TABLE TopProducts AS
SELECT
    product_name,
    store_name,
    total_revenue,
    RANK() OVER (PARTITION BY store_name ORDER BY total_revenue DESC) AS product_rank
FROM
    ProductRevenue;

-- Retrieve the final results: store-wise product performance and category-wise average revenue
SELECT
    pr.product_name,
    pr.store_name,
    pr.total_revenue,
    cr.avg_category_revenue
FROM
    TopProducts pr
JOIN
    CategoryRevenue cr ON pr.product_category = cr.product_category
WHERE
    product_rank <= 3;

-- Drop the temporary tables
DROP TABLE IntermediateResults;
DROP TABLE ProductRevenue;
DROP TABLE CategoryRevenue;
DROP TABLE TopProducts;
```

Explanation:

1. Create Temporary Tables:
    - `IntermediateResults`: Stores intermediate data by joining `Products`, `Stores`, and `Orders` tables.
    - `ProductRevenue`: Calculates the total revenue for each product in each store.
    - `CategoryRevenue`: Calculates the average revenue for each product category across all stores.
    - `TopProducts`: Ranks products within each store based on total revenue.

2. Final Result Query:
    - Selects product name, store name, total revenue, and average category revenue.
    - Joins `TopProducts` with `CategoryRevenue` to include category-wise average revenue.
    - Filters for products ranked within the top 3 in each store.

3. Drop Temporary Tables:
    - After retrieving the final results, drop the temporary tables to clean up the database.