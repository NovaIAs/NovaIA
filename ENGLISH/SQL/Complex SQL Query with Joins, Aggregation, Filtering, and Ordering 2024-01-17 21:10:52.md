```sql
-- This is a complex SQL query that retrieves data from multiple tables and performs various operations, including joining, aggregation, and filtering.

SELECT
    t1.column1,
    t1.column2,
    t2.column3,
    t2.column4,
    SUM(t3.column5) AS total_sales
FROM
    table1 AS t1
JOIN
    table2 AS t2
ON
    t1.id = t2.id
LEFT JOIN
    table3 AS t3
ON
    t2.id = t3.id
WHERE
    t1.column1 = 'value1'
    AND t2.column3 > 100
    AND t3.column5 IS NOT NULL
GROUP BY
    t1.column1,
    t1.column2,
    t2.column3,
    t2.column4
ORDER BY
    total_sales DESC
LIMIT 10;

-- Explanation:

-- The SELECT clause specifies the columns to be retrieved from the tables.

-- The FROM clause specifies the tables to be joined in the query. In this case, three tables are joined: table1, table2, and table3.

-- The JOIN clause specifies how the tables are to be joined. In this case, table1 and table2 are joined on the id column, and table2 and table3 are joined on the id column.

-- The WHERE clause specifies the conditions that the rows in the tables must satisfy in order to be included in the results. In this case, the rows in table1 must have a column1 value of 'value1', the rows in table2 must have a column3 value greater than 100, and the rows in table3 must have a column5 value that is not null.

-- The GROUP BY clause specifies the columns by which the results should be grouped. In this case, the results are grouped by the column1 and column2 columns of table1, and the column3 and column4 columns of table2.

-- The ORDER BY clause specifies the order in which the results should be returned. In this case, the results are ordered by the total_sales column in descending order.

-- The LIMIT clause specifies the maximum number of rows to be returned. In this case, the query will return a maximum of 10 rows.
```