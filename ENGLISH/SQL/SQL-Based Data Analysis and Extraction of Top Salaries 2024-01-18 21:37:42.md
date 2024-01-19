```sql
-- This complex SQL code performs extensive data processing and analysis on a large dataset.

-- First, we create a temporary table called `tmp_joined_data` by joining two tables, `table1` and `table2`, using an INNER JOIN on the common column `id`.
CREATE TEMP TABLE tmp_joined_data AS
SELECT
    t1.id,
    t1.name,
    t1.age,
    t2.salary
FROM
    table1 AS t1
INNER JOIN
    table2 AS t2
ON
    t1.id = t2.id;

-- Next, we calculate the average salary for each unique name in the `tmp_joined_data` table and store the result in another temporary table called `tmp_avg_salary`.
CREATE TEMP TABLE tmp_avg_salary AS
SELECT
    name,
    AVG(salary) AS average_salary
FROM
    tmp_joined_data
GROUP BY
    name;

-- We then create a third temporary table called `tmp_top_salaries` to identify the top 10 names with the highest average salaries.
CREATE TEMP TABLE tmp_top_salaries AS
SELECT
    name,
    average_salary
FROM
    tmp_avg_salary
ORDER BY
    average_salary DESC
LIMIT 10;

-- Finally, we retrieve the data from the `tmp_top_salaries` table and display the results in a user-friendly format.
SELECT
    name,
    average_salary
FROM
    tmp_top_salaries;

-- Drop the temporary tables to free up resources.
DROP TABLE tmp_joined_data;
DROP TABLE tmp_avg_salary;
DROP TABLE tmp_top_salaries;
```

Explanation of the code:

1. **Temporary Tables:** The code utilizes temporary tables (`tmp_joined_data`, `tmp_avg_salary`, and `tmp_top_salaries`) to store intermediate results during the data processing. This approach is employed to improve performance and reduce complexity.

2. **Table Joining:** An INNER JOIN is performed between `table1` and `table2` on the `id` column using the `CREATE TEMP TABLE tmp_joined_data` statement. This operation merges data from the two tables based on matching `id` values.

3. **Aggregation:** The `CREATE TEMP TABLE tmp_avg_salary` statement calculates the average salary for each unique name in the `tmp_joined_data` table using the `AVG()` aggregate function. The `GROUP BY` clause groups the data by name, ensuring that the average salary is calculated separately for each distinct name.

4. **Top Salaries:** The `CREATE TEMP TABLE tmp_top_salaries` statement identifies the top 10 names with the highest average salaries from the `tmp_avg_salary` table. It achieves this by sorting the data in descending order based on average salary and selecting the top 10 records using the `ORDER BY` and `LIMIT` clauses.

5. **Result Retrieval:** The final `SELECT` statement retrieves the data from the `tmp_top_salaries` table and displays the names and corresponding average salaries in a user-friendly format.

6. **Temporary Table Cleanup:** The temporary tables (`tmp_joined_data`, `tmp_avg_salary`, and `tmp_top_salaries`) are dropped using the `DROP TABLE` statements to free up the resources they were occupying.

This complex SQL code demonstrates advanced data processing techniques, including table joining, aggregation, sorting, and data filtering, to extract meaningful insights from a large dataset.