```sql
-- This query retrieves data about customers and their orders, including the total amount spent by each customer.

SELECT
    c.customer_id,                -- The unique customer ID
    c.first_name,                 -- The first name of the customer
    c.last_name,                  -- The last name of the customer
    o.order_id,                   -- The unique order ID
    o.order_date,                 -- The date when the order was placed
    od.product_id,                -- The unique product ID
    p.product_name,               -- The name of the product
    od.quantity,                  -- The quantity of the product ordered
    od.unit_price,                -- The unit price of the product
    (od.quantity * od.unit_price) AS total_price -- The total price of the individual product ordered

-- From the following tables:
FROM
    customers c                   -- The 'customers' table
JOIN
    orders o                     -- The 'orders' table
ON
    c.customer_id = o.customer_id -- Join the two tables on the 'customer_id' column

-- Perform a LEFT JOIN with the 'order_details' table on the 'order_id' column
LEFT JOIN
    order_details od
ON
    o.order_id = od.order_id

-- Perform another LEFT JOIN with the 'products' table on the 'product_id' column
LEFT JOIN
    products p
ON
    od.product_id = p.product_id

-- Group the results by the customer ID, order ID, and product ID to aggregate the data
GROUP BY
    c.customer_id,
    o.order_id,
    od.product_id

-- Order the results by the customer ID, order date, and product name
ORDER BY
    c.customer_id,
    o.order_date,
    p.product_name;

-- End of the SQL query;
```

Explanation:

- The query starts with the SELECT statement, which specifies the columns to be retrieved from the database. In this case, it selects various columns from the 'customers', 'orders', 'order_details', and 'products' tables.

- The FROM statement specifies the tables to be used in the query: 'customers', 'orders', 'order_details', and 'products'.

- The JOIN statements are used to combine data from multiple tables based on common columns. The query performs a LEFT JOIN between the 'customers' and 'orders' tables on the 'customer_id' column, a LEFT JOIN between the 'orders' and 'order_details' tables on the 'order_id' column, and a LEFT JOIN between the 'order_details' and 'products' tables on the 'product_id' column.

- The LEFT JOIN operation ensures that all rows from the left table ('customers' or 'orders') are included in the result, even if there are no matching rows in the right table ('orders', 'order_details', or 'products').

- The GROUP BY statement is used to group the results by the customer ID, order ID, and product ID. This allows the query to aggregate the data for each customer, order, and product combination.

- The ORDER BY statement is used to order the results by the customer ID, order date, and product name.

- The query calculates the total price for each individual product ordered by multiplying the quantity ordered by the unit price. The result is aliased as 'total_price'.

- The query uses parentheses around the expression '(od.quantity * od.unit_price)' to ensure that the multiplication operation is performed before the addition operation in the SELECT statement.

- The query is terminated with a semicolon (;).