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
        AVG(sales) OVER (PARTITION BY product_id ORDER BY date ROWS BETWEEN 6 PRECEDING AND CURRENT ROW) AS moving_average
    FROM sales_data
)

-- Select the product ID, date, sales, cumulative sales, and moving average for each row
SELECT
    product_id,
    date,
    sales,
    cumulative_sales,
    moving_average
FROM sales_data

-- Join the temporary tables CumulativeSales and MovingAverage on product_id
INNER JOIN CumulativeSales ON sales_data.product_id = CumulativeSales.product_id
INNER JOIN MovingAverage ON sales_data.product_id = MovingAverage.product_id

-- Filter the results to include only rows where the moving average is greater than 100
WHERE moving_average > 100

-- Order the results by product ID and date
ORDER BY product_id, date;
```

Explanation:

1. The WITH statement is used to create two temporary tables, CumulativeSales and MovingAverage, which are used in the subsequent SELECT statement.

2. The CumulativeSales temporary table is created using a window function (SUM() OVER ()) to calculate the cumulative sum of sales for each product.

3. The MovingAverage temporary table is created using a window function (AVG() OVER ()) to calculate the moving average of sales for each product over a window of the last 7 days (including the current day).

4. The SELECT statement is used to select the product ID, date, sales, cumulative sales, and moving average for each row in the sales_data table.

5. The INNER JOIN statements are used to join the sales_data table with the CumulativeSales and MovingAverage temporary tables on the product_id column.

6. The WHERE statement is used to filter the results to include only rows where the moving average is greater than 100.

7. The ORDER BY statement is used to order the results by product ID and date.