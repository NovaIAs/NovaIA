```sql
-- Create a temporary table to store the hierarchical data.

WITH RecursiveHierarchy AS (
    SELECT
        Id,
        ParentId,
        Name,
        Level = 0
    FROM
        Categories
    WHERE
        ParentId IS NULL

    UNION ALL

    SELECT
        c.Id,
        c.ParentId,
        c.Name,
        rh.Level + 1
    FROM
        Categories c
    INNER JOIN
        RecursiveHierarchy rh ON c.ParentId = rh.Id
)

-- Select the data from the temporary table and format it as a hierarchy.

SELECT
    Id,
    ParentId,
    Name,
    Level
FROM
    RecursiveHierarchy

-- Order the results by the level of the hierarchy.

ORDER BY
    Level;

```

Explanation:

1. Create a temporary table called RecursiveHierarchy using a recursive Common Table Expression (CTE) WITH clause.

2. The base case of the recursion (SELECT statement inside the WITH clause) retrieves all the top-level categories (ParentId IS NULL) and sets their Level to 0.

3. The recursive part of the CTE (UNION ALL clause) appends rows for child categories to the temporary table. It joins the Categories table with the RecursiveHierarchy table on the ParentId column and increments the Level column by 1 for each level of nesting.

4. Select the Id, ParentId, Name, and Level columns from the RecursiveHierarchy temporary table.

5. Order the results by the Level column to display the categories in a hierarchical structure.