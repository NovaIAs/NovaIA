```sql
-- Create a temporary table to store the cumulative sum of sales for each product
WITH CumulativeSales AS (
    SELECT
        product_id,
        SUM(sales) OVER (PARTITION BY product_id ORDER BY date) AS cumulative_sales
    FROM sales_data
)

-- Create a temporary table to store the moving average of sales for each product
WITH MovingAverage AS (
    SELECT
        product_id,
        AVG(sales) OVER (PARTITION BY product_id ORDER BY date ROWS BETWEEN 4 PRECEDING AND CURRENT ROW) AS moving_average
    FROM sales_data
)

-- Select the product ID, cumulative sales, and moving average for each product
SELECT
    product_id,
    cumulative_sales,
    moving_average
FROM CumulativeSales
JOIN MovingAverage ON CumulativeSales.product_id = MovingAverage.product_id;
```

Explanation:

1. The `CumulativeSales` CTE (Common Table Expression) calculates the cumulative sum of sales for each product. It uses the `SUM()` function with the `OVER()` clause to calculate the cumulative sum for each product, partitioned by the `product_id` column and ordered by the `date` column.

2. The `MovingAverage` CTE calculates the moving average of sales for each product. It uses the `AVG()` function with the `OVER()` clause to calculate the moving average for each product, partitioned by the `product_id` column and ordered by the `date` column, with a window size of 5 rows (the current row and the 4 preceding rows).

3. The final query joins the `CumulativeSales` and `MovingAverage` CTEs on the `product_id` column to retrieve the product ID, cumulative sales, and moving average for each product.