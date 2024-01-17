```sql
-- This SQL query retrieves data from multiple tables and combines them using various join operations to provide a comprehensive view of customer information, orders, and products.

-- Start by selecting various columns from the 'customers' table.
SELECT
    c.customer_id,                -- Customer's unique identifier
    c.first_name,                 -- Customer's first name
    c.last_name,                  -- Customer's last name
    c.email,                     -- Customer's email address
    c.phone_number,              -- Customer's phone number
    c.address,                   -- Customer's address
    c.city,                      -- Customer's city
    c.state,                     -- Customer's state
    c.zip_code,                  -- Customer's zip code
    
    -- Join the 'customers' table with the 'orders' table using 'customer_id' as the join key.
    o.order_id,                  -- Order's unique identifier
    o.order_date,                -- Date when the order was placed
    o.total_amount,              -- Total amount of the order
    o.status,                    -- Current status of the order (e.g., 'Processing', 'Shipped', 'Delivered')
    
    -- Perform a left join between the 'orders' table and the 'order_items' table using 'order_id' as the join key.
    oi.product_id,               -- Product's unique identifier
    oi.quantity,                 -- Quantity of the product ordered
    oi.unit_price,               -- Unit price of the product
    
    -- Join the 'order_items' table with the 'products' table using 'product_id' as the join key.
    p.name,                      -- Product's name
    p.description,               -- Product's description
    p.category,                  -- Product's category
    p.brand,                     -- Product's brand
    p.price                      -- Product's price

-- Specify the tables to join: 'customers' (aliased as 'c'), 'orders' (aliased as 'o'), 'order_items' (aliased as 'oi'), and 'products' (aliased as 'p').
FROM
    customers c
JOIN
    orders o ON c.customer_id = o.customer_id
LEFT JOIN
    order_items oi ON o.order_id = oi.order_id
JOIN
    products p ON oi.product_id = p.product_id;
```

Explanation:

1. **SELECT Clause**: This clause specifies the columns to be retrieved from the joined tables. It includes customer information, order information, and product information.

2. **Join Operations**: The query uses a combination of INNER JOIN and LEFT JOIN to connect the 'customers', 'orders', 'order_items', and 'products' tables based on their relationships:
   - **INNER JOIN**: The 'customers' and 'orders' tables are joined using an INNER JOIN on the 'customer_id' column. This ensures that only customers with at least one order are included in the results.
   - **LEFT JOIN**: The 'orders' and 'order_items' tables are joined using a LEFT JOIN on the 'order_id' column. This allows for the inclusion of orders even if they don't have any associated order items.
   - **INNER JOIN**: The 'order_items' and 'products' tables are joined using an INNER JOIN on the 'product_id' column. This ensures that only products that are part of an order are included in the results.

3. **Result**: The query combines data from all four tables, allowing you to view customer information, order details, and product information in a single result set. This provides a comprehensive overview of customer activity and purchasing patterns.