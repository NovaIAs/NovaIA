```sql
-- Create a temporary table to store the results of a complex query
CREATE TEMP TABLE tmp_results AS (
    -- Select the company name and the sum of sales for each company
    SELECT company_name, SUM(sales) AS total_sales
    -- From the sales table
    FROM sales
    -- Group the results by company name
    GROUP BY company_name
    -- Having a total sales greater than or equal to $1,000,000
    HAVING total_sales >= 1000000
);

-- Select the company name, total sales, and the difference between the total sales and the average total sales
SELECT company_name, total_sales, (total_sales - AVG(total_sales)) AS sales_difference
-- From the temporary table
FROM tmp_results
-- Join the temporary table with itself on the company name
INNER JOIN tmp_results ON tmp_results.company_name = tmp_results.company_name
-- Where the company names are not equal
WHERE tmp_results.company_name <> tmp_results.company_name
-- Order the results by the sales difference in descending order
ORDER BY sales_difference DESC;

-- Drop the temporary table
DROP TABLE tmp_results;
```

Explanation:

This SQL code performs a complex analysis on a sales table to identify companies with high sales and significant differences in their sales compared to the average sales of other companies. It involves creating a temporary table, performing calculations, joining the temporary table with itself, and filtering the results.

The code begins by creating a temporary table called `tmp_results` using the `CREATE TEMP TABLE` statement. This table is used to store the results of a complex query that calculates the total sales for each company and filters the results to include companies with total sales greater than or equal to $1,000,000.

Next, the code performs a series of calculations on the `tmp_results` table. It calculates the average total sales using the `AVG()` function and then joins the `tmp_results` table with itself using the `INNER JOIN` statement. This join allows the code to compare the total sales of each company with the average total sales of other companies.

The code then filters the results using the `WHERE` clause to include only rows where the company names are not equal. This ensures that the code only compares the sales of different companies.

Finally, the `ORDER BY` clause is used to sort the results by the sales difference in descending order. This allows the code to identify the companies with the largest differences between their total sales and the average total sales.

The results of the query include the company name, total sales, and the difference between the total sales and the average total sales for each company. This information can be used to identify companies that are performing exceptionally well or poorly compared to their peers.