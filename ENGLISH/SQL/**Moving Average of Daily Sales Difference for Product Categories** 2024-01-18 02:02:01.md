```sql
-- Create a temporary table to store the cumulative sum of all sales by product category for each day
WITH CumulativeSales AS (
    SELECT
        product_category,
        DATE(sale_date) AS sale_day,
        SUM(sale_amount) OVER (PARTITION BY product_category ORDER BY sale_date) AS cumulative_sales
    FROM
        sales_data
)

-- Create a temporary table to store the daily difference in cumulative sales for each product category
WITH DailySalesDifference AS (
    SELECT
        product_category,
        sale_day,
        cumulative_sales - LAG(cumulative_sales, 1, 0) OVER (PARTITION BY product_category ORDER BY sale_day) AS daily_sales_difference
    FROM
        CumulativeSales
)

-- Create a temporary table to store the moving average of daily sales difference for each product category over a 7-day window
WITH MovingAverage AS (
    SELECT
        product_category,
        sale_day,
        AVG(daily_sales_difference) OVER (PARTITION BY product_category ORDER BY sale_day ASC ROWS BETWEEN 6 PRECEDING AND CURRENT ROW) AS moving_average
    FROM
        DailySalesDifference
)

-- Select the product category, sale day, and moving average for the last 7 days
SELECT
    product_category,
    sale_day,
    moving_average
FROM
    MovingAverage
WHERE
    sale_day >= DATE('now', '-6 days')

-- Order the results by product category and sale day
ORDER BY
    product_category,
    sale_day;
```

This SQL code performs the following operations:

1. It creates a temporary table called `CumulativeSales` that calculates the cumulative sum of sales for each product category for each day.

2. It creates a temporary table called `DailySalesDifference` that calculates the daily difference in cumulative sales for each product category.

3. It creates a temporary table called `MovingAverage` that calculates the moving average of the daily sales difference for each product category over a 7-day window.

4. It selects the product category, sale day, and moving average for the last 7 days.

5. It orders the results by product category and sale day.

This code can be used to analyze sales trends and identify patterns in sales data. It can also be used to make predictions about future sales based on historical data.