```sql
-- Create a temporary table to store the results of a complex query
WITH tmp AS (
  -- Select specific columns from the 'sales' table
  SELECT
    s.product_id,
    s.customer_id,
    s.sales_date,
    s.sales_amount,
    -- Calculate the year and month components of the sales date
    strftime('%Y', s.sales_date) AS sales_year,
    strftime('%m', s.sales_date) AS sales_month
  FROM sales s

  -- Filter the results to include only specific product categories and years
  WHERE s.product_category IN ('Electronics', 'Clothing')
    AND strftime('%Y', s.sales_date) BETWEEN '2018' AND '2022'
),

-- Create a temporary table to calculate cumulative sales for each year and month
tmp2 AS (
  -- Group the sales data by year, month, and customer ID
  SELECT
    sales_year,
    sales_month,
    customer_id,
    -- Calculate the cumulative sum of sales amount for each customer
    SUM(sales_amount) OVER (PARTITION BY customer_id ORDER BY sales_date) AS cumulative_sales
  FROM tmp
),

-- Create a temporary table to calculate average sales for each year and month
tmp3 AS (
  -- Group the sales data by year, month, and customer ID
  SELECT
    sales_year,
    sales_month,
    customer_id,
    -- Calculate the average sales amount for each customer
    AVG(sales_amount) OVER (PARTITION BY customer_id ORDER BY sales_date) AS average_sales
  FROM tmp
)

-- Select the desired columns from the temporary tables and join them together
SELECT
  tmp.product_id,
  tmp.customer_id,
  tmp.sales_date,
  tmp.sales_amount,
  tmp.sales_year,
  tmp.sales_month,
  tmp2.cumulative_sales,
  tmp3.average_sales
FROM tmp
INNER JOIN tmp2 ON tmp.sales_year = tmp2.sales_year
                 AND tmp.sales_month = tmp2.sales_month
                 AND tmp.customer_id = tmp2.customer_id
INNER JOIN tmp3 ON tmp.sales_year = tmp3.sales_year
                 AND tmp.sales_month = tmp3.sales_month
                 AND tmp.customer_id = tmp3.customer_id;
```

**Explanation:**

This complex SQL code performs a series of operations to analyze sales data. It involves creating temporary tables, calculating cumulative and average sales, and then joining the results together.

1. The first temporary table, `tmp`, is created by selecting specific columns from the `sales` table. It includes the product ID, customer ID, sales date, sales amount, and year and month components of the sales date. The query is filtered to include only sales from specific product categories and years.

2. The second temporary table, `tmp2`, is created to calculate the cumulative sales for each year, month, and customer ID. It groups the sales data by year, month, and customer ID, and then calculates the cumulative sum of sales amount for each customer using the `SUM()` function with the `OVER()` clause.

3. The third temporary table, `tmp3`, is created to calculate the average sales for each year, month, and customer ID. It groups the sales data by year, month, and customer ID, and then calculates the average sales amount for each customer using the `AVG()` function with the `OVER()` clause.

4. Finally, the `SELECT` statement at the end of the code joins the three temporary tables together using `INNER JOIN` and selects the desired columns from each table. This allows you to retrieve the product ID, customer ID, sales date, sales amount, year, month, cumulative sales, and average sales for each sales record that meets the specified criteria.

This complex SQL code demonstrates advanced data analysis techniques and can be used to gain insights into sales trends and customer behavior over time.