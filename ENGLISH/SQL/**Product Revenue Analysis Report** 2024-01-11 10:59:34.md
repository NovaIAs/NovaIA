```sql
-- Common Table Expression (CTE) to calculate the total revenue for each product category
WITH ProductRevenue AS (
    SELECT
        p.category_id,
        SUM(oi.quantity * oi.unit_price) AS total_revenue
    FROM
        products p
    JOIN
        order_items oi ON p.product_id = oi.product_id
    GROUP BY
        p.category_id
)

-- Main query to retrieve product details along with the calculated total revenue for each category
SELECT
    p.product_id,
    p.product_name,
    p.unit_price,
    c.category_name,
    COALESCE(pr.total_revenue, 0) AS total_revenue
FROM
    products p
JOIN
    categories c ON p.category_id = c.category_id
LEFT JOIN
    ProductRevenue pr ON p.category_id = pr.category_id
ORDER BY
    p.product_id;
```

**Explanation:**

1. **Common Table Expression (CTE): ProductRevenue:**
   - This CTE calculates the total revenue for each product category.
   - It joins the 'products' table with the 'order_items' table based on the 'product_id' column.
   - It groups the results by the 'category_id' and calculates the sum of the product of 'quantity' and 'unit_price' for each category, which represents the total revenue for that category.

2. **Main Query:**
   - The main query retrieves detailed information about each product, including its 'product_id', 'product_name', 'unit_price', and 'category_name'.
   - It performs a LEFT JOIN with the 'ProductRevenue' CTE using the 'category_id' column to link products with their respective category's total revenue.
   - The 'COALESCE' function is used to handle null values in the 'total_revenue' column, replacing them with 0 if null.
   - The results are ordered by the 'product_id'.

This complex SQL code combines a Common Table Expression (CTE) with a main query to efficiently calculate and display product-related information, including total revenue for each product category. It demonstrates advanced data manipulation and aggregation techniques in SQL.