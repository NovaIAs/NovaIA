```sql
-- Create a temporary table to store the results of a complex query
CREATE TEMP TABLE tmp_results AS
(
    -- Select the product name, unit price, and quantity sold for each product
    SELECT p.product_name, p.unit_price, o.quantity_sold
    FROM products p
    JOIN order_items o ON p.product_id = o.product_id
    
    -- Filter the results to include only products with a unit price greater than $10
    WHERE p.unit_price > 10
    
    -- Group the results by product name
    GROUP BY p.product_name
    
    -- Calculate the total revenue for each product
    HAVING SUM(o.quantity_sold * p.unit_price) > 10000
);

-- Create a common table expression (CTE) to calculate the average revenue for products in each category
WITH CategoryRevenue AS
(
    -- Select the category name and the average revenue for products in that category
    SELECT c.category_name, AVG(o.quantity_sold * p.unit_price) AS avg_revenue
    FROM categories c
    JOIN products p ON c.category_id = p.category_id
    JOIN order_items o ON p.product_id = o.product_id
    
    -- Group the results by category name
    GROUP BY c.category_name
),

-- Create another CTE to calculate the total revenue for all products
TotalRevenue AS
(
    -- Select the sum of the revenue for all products
    SELECT SUM(o.quantity_sold * p.unit_price) AS total_revenue
    FROM products p
    JOIN order_items o ON p.product_id = o.product_id
)

-- Select the product name, unit price, quantity sold, category name, average revenue for the category, and total revenue for all products
SELECT tmp_results.product_name, tmp_results.unit_price, tmp_results.quantity_sold, c.category_name, CategoryRevenue.avg_revenue, TotalRevenue.total_revenue
FROM tmp_results
JOIN categories c ON tmp_results.product_name = c.category_name
JOIN CategoryRevenue ON c.category_name = CategoryRevenue.category_name
JOIN TotalRevenue ON 1=1;

-- Drop the temporary table
DROP TABLE tmp_results;
```

Explanation:

1. **Create Temporary Table (tmp_results):**
   - This temporary table stores the results of a complex query that includes product names, unit prices, quantities sold, and filters products with a unit price greater than $10.
   - It groups the results by product name and calculates the total revenue for each product using `HAVING SUM(...) > 10000`.

2. **Common Table Expression (CategoryRevenue):**
   - This CTE calculates the average revenue for products in each category.
   - It joins the `categories`, `products`, and `order_items` tables and groups the results by category name.
   - It then calculates the average revenue using `AVG(o.quantity_sold * p.unit_price)`.

3. **Common Table Expression (TotalRevenue):**
   - This CTE calculates the total revenue for all products.
   - It joins the `products` and `order_items` tables and calculates the sum of the revenue for all products.

4. **Final Select Query:**
   - This query selects various columns from the `tmp_results` table, including product name, unit price, quantity sold, category name, average revenue for the category, and total revenue for all products.
   - It joins the `tmp_results` table with the `categories` table, `CategoryRevenue` CTE, and `TotalRevenue` CTE to retrieve the necessary data.

5. **Drop Temporary Table:**
   - After the query is executed, the `tmp_results` temporary table is dropped since it is no longer needed.

This complex SQL code demonstrates the use of temporary tables, common table expressions (CTEs), joins, grouping, aggregation functions, and subqueries to retrieve and analyze data from multiple tables.