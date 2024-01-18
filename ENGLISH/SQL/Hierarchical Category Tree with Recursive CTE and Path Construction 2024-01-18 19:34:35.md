```sql
WITH RecursiveCTE AS (
    SELECT
        ID,
        ParentID,
        Name,
        Level
    FROM
        Categories
    WHERE
        ParentID IS NULL

    UNION

    SELECT
        c.ID,
        c.ParentID,
        c.Name,
        rc.Level + 1 AS Level
    FROM
        Categories c
    JOIN
        RecursiveCTE rc ON c.ParentID = rc.ID
)

SELECT
    rc.ID,
    rc.ParentID,
    rc.Name,
    rc.Level,
    (
        SELECT GROUP_CONCAT(c2.Name, ' > ')
        FROM Categories c2
        WHERE c2.ID IN (
            SELECT rc3.ID
            FROM RecursiveCTE rc3
            WHERE rc3.Level BETWEEN rc.Level AND 0
        )
        ORDER BY c2.Level DESC
    ) AS Path
FROM
    RecursiveCTE rc;
```

Explanation:

This complex SQL code uses a recursive common table expression (CTE) to achieve the following:

1. It starts with a base query that selects the top-level categories (where `ParentID` is `NULL`) and includes columns for `ID`, `ParentID`, `Name`, and `Level` (initially set to 0).

2. The recursive part of the CTE uses a `UNION` to add rows for child categories. It joins the `Categories` table with the `RecursiveCTE` on the `ParentID` column. This allows the CTE to traverse the category hierarchy recursively, adding child categories at each level.

3. The `Level` column is incremented by 1 for each level of recursion, representing the depth of each category in the hierarchy.

4. The main `SELECT` statement retrieves information about each category, including its `ID`, `ParentID`, `Name`, and `Level`.

5. It also includes a subquery that uses another recursive CTE to construct the category path for each category. The subquery concatenates the names of all parent categories, starting from the current category and going up to the top-level category.

6. Finally, the `Path` column contains the complete category path for each category, separated by `>`.

This code can be used to generate a hierarchical representation of categories or any other tree-structured data in a database.