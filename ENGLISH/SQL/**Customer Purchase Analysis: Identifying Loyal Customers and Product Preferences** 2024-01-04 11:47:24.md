```sql
-- This query retrieves data from a table called 'customers' and performs various operations on the data.

SELECT
    c.customer_id,            -- Select the 'customer_id' column.
    c.first_name,             -- Select the 'first_name' column.
    c.last_name,              -- Select the 'last_name' column.
    c.email,                 -- Select the 'email' column.
    c.phone_number,           -- Select the 'phone_number' column.
    c.address,                -- Select the 'address' column.
    c.city,                  -- Select the 'city' column.
    c.state,                 -- Select the 'state' column.
    c.zip_code,              -- Select the 'zip_code' column.
    c.date_of_birth,         -- Select the 'date_of_birth' column.
    c.gender,                -- Select the 'gender' column.
    o.order_id,              -- Select the 'order_id' column from the 'orders' table.
    o.order_date,            -- Select the 'order_date' column from the 'orders' table.
    o.total_amount,          -- Select the 'total_amount' column from the 'orders' table.
    oi.product_id,           -- Select the 'product_id' column from the 'order_items' table.
    oi.quantity,             -- Select the 'quantity' column from the 'order_items' table.
    p.product_name,          -- Select the 'product_name' column from the 'products' table.
    p.unit_price             -- Select the 'unit_price' column from the 'products' table.

-- This query joins four tables: 'customers', 'orders', 'order_items', and 'products'.

FROM
    customers c               -- Alias the 'customers' table as 'c'.
JOIN
    orders o                 -- Join the 'orders' table using the 'customer_id' column.
ON
    c.customer_id = o.customer_id
JOIN
    order_items oi            -- Join the 'order_items' table using the 'order_id' column.
ON
    o.order_id = oi.order_id
JOIN
    products p               -- Join the 'products' table using the 'product_id' column.
ON
    oi.product_id = p.product_id

-- This query filters the results to include only customers who have placed an order in the last 30 days.

WHERE
    o.order_date >= DATE('now', '-30 days')

-- This query groups the results by the 'customer_id' and 'product_id' columns.

GROUP BY
    c.customer_id,
    p.product_id

-- This query calculates the total quantity ordered for each customer and product combination.

HAVING
    SUM(oi.quantity) > 10;  -- Filter results to include only those with a total quantity greater than 10.

-- This query orders the results by the 'total_amount' column in descending order.

ORDER BY
    o.total_amount DESC;
```

**Explanation:**

This SQL query performs a complex data retrieval and aggregation operation across four tables: 'customers', 'orders', 'order_items', and 'products'. It retrieves customer information, order details, and product information, and then groups and aggregates the data to provide insights into customer purchase behavior.

**Key Features:**

1. **Table Joins:** The query uses INNER JOINs to connect four tables: 'customers', 'orders', 'order_items', and 'products'. This allows it to retrieve data from multiple tables and combine them based on common columns.

2. **Date Filtering:** The query includes a WHERE clause that filters the results to include only orders placed in the last 30 days. This helps focus the analysis on recent customer activity.

3. **Grouping and Aggregation:** The query uses GROUP BY and HAVING clauses to group the results by the 'customer_id' and 'product_id' columns and calculate the total quantity ordered for each customer and product combination. The HAVING clause then filters the results to include only those combinations with a total quantity greater than 10.

4. **Sorting:** The query uses an ORDER BY clause to sort the results by the 'total_amount' column in descending order. This allows you to identify the customer and product combinations with the highest total sales.

**Result:**

The query returns a result set that includes customer information, order details, and product information for each customer who has placed an order in the last 30 days and has purchased a specific product combination with a total quantity greater than 10. This information can be used for various purposes, such as identifying loyal customers, analyzing customer preferences, and optimizing product recommendations.