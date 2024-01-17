```sql
WITH RecursiveCTE AS (
    SELECT
        id,
        parent_id,
        name,
        level
    FROM
        categories
    WHERE
        level = 0
    UNION ALL
    SELECT
        c.id,
        c.parent_id,
        c.name,
        rc.level + 1
    FROM
        RecursiveCTE rc
    JOIN
        categories c ON rc.id = c.parent_id
)

SELECT
    rc.id,
    rc.parent_id,
    rc.name,
    rc.level
FROM
    RecursiveCTE rc
ORDER BY
    rc.level, rc.id;
```

Explanation:

This SQL code uses a recursive common table expression (CTE) to perform a hierarchical query on a table of categories. The CTE is defined as follows:

* The base case of the recursion is selected from the `categories` table where the `level` column is 0. This represents the top-level categories.
* The recursive part of the CTE selects child categories by joining the CTE with itself on the `parent_id` column. The `level` column is incremented by 1 for each level of recursion.

The final `SELECT` statement retrieves the columns `id`, `parent_id`, `name`, and `level` from the CTE and orders the results by `level` and `id`.

This code can be used to generate a hierarchical list of categories, such as a menu or a site navigation structure.