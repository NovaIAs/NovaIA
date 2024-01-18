```sql
WITH RecursiveQuery AS (
    SELECT
        o.order_id,
        o.customer_name,
        s.product_name,
        s.quantity,
        c.currency_code,
        (s.quantity * c.exchange_rate) AS revenue_in_base_currency
    FROM
        orders AS o
    JOIN
        order_items AS s ON o.order_id = s.order_id
    JOIN
        products AS p ON s.product_name = p.product_name
    JOIN
        currency_exchange_rates AS c ON p.currency_code = c.currency_code
    WHERE
        o.order_date >= '2020-01-01' AND o.order_date <= '2020-12-31'
),
SummarizedData AS (
    SELECT
        customer_name,
        product_name,
        SUM(quantity) AS total_quantity,
        SUM(revenue_in_base_currency) AS total_revenue_in_base_currency
    FROM
        RecursiveQuery
    GROUP BY
        customer_name,
        product_name
)
SELECT
    customer_name,
    product_name,
    total_quantity,
    total_revenue_in_base_currency,
    RANK() OVER (PARTITION BY customer_name ORDER BY total_revenue_in_base_currency DESC) AS rank
FROM
    SummarizedData
WHERE
    rank <= 3;
```

Explanation:

1. **RecursiveQuery**: This CTE (Common Table Expression) retrieves data from the 'orders', 'order_items', 'products', and 'currency_exchange_rates' tables for orders placed between '2020-01-01' and '2020-12-31'. It calculates the revenue for each order item in the base currency using the exchange rates.

2. **SummarizedData**: This CTE aggregates the data from the 'RecursiveQuery' CTE by customer name and product name. It calculates the total quantity and total revenue for each customer-product pair.

3. The main query:
   - Selects customer names, product names, total quantities, and total revenues for each customer-product pair.
   - Uses the RANK() function to rank customer-product pairs based on total revenue in descending order for each customer. Only the top three customer-product pairs are included in the results.

This complex SQL code allows you to analyze customer purchases and identify the top-selling products for each customer, considering currency exchange rates and time periods.