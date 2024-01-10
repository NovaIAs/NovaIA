```sql
-- Create a temporary table to store the results of a complex query
CREATE TEMP TABLE tmp_results AS
(
    -- Select various columns from different tables, including some calculations and aggregations
    SELECT
        t1.column1,
        t2.column2,
        t3.column3,
        SUM(t4.column4) AS total_column4,
        AVG(t5.column5) AS avg_column5
    
    -- Join the tables using multiple INNER JOINs
    FROM table1 t1
    INNER JOIN table2 t2 ON t1.id = t2.id
    INNER JOIN table3 t3 ON t2.id = t3.id
    INNER JOIN table4 t4 ON t3.id = t4.id
    INNER JOIN table5 t5 ON t4.id = t5.id
    
    -- Filter the results based on certain conditions
    WHERE t1.column1 > 100
    AND t2.column2 < 50
    AND t3.column3 = 'XYZ'
    
    -- Group the results by specific columns
    GROUP BY t1.column1, t2.column2, t3.column3
);

-- Select and display the results from the temporary table
SELECT
    tmp_results.column1,
    tmp_results.column2,
    tmp_results.column3,
    tmp_results.total_column4,
    tmp_results.avg_column5
FROM tmp_results;

-- Drop the temporary table after use
DROP TABLE tmp_results;
```

Explanation:

1. **Create Temporary Table**: The first step is to create a temporary table called `tmp_results` using the `CREATE TEMP TABLE` statement. This table will store the results of a complex query.

2. **Complex Query**: The query inside the parentheses is a complex SQL statement that retrieves data from multiple tables and performs various operations, including calculations, aggregations, filtering, and grouping.

3. **Table Joins**: The query uses multiple `INNER JOIN` statements to join the tables based on their relationships. This allows us to combine data from different tables into a single result set.

4. **Filtering and Grouping**: The query includes `WHERE` and `GROUP BY` clauses to filter the results based on specific conditions and group the data by certain columns. This helps in narrowing down the results and organizing the data in a meaningful way.

5. **Calculations and Aggregations**: The query performs calculations and aggregations using functions like `SUM()` and `AVG()` to calculate the total value of a column and the average value of another column, respectively.

6. **Selecting and Displaying Results**: After the complex query is executed, the `SELECT` statement is used to select specific columns from the `tmp_results` table and display them as the final result set.

7. **Dropping Temporary Table**: Finally, the `DROP TABLE` statement is used to drop the `tmp_results` table after it has served its purpose. This is done to free up resources and avoid clutter in the database.