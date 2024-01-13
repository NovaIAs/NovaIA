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
        level = 1

    UNION ALL

    SELECT
        c.id,
        c.parent_id,
        c.name,
        rcte.level + 1
    FROM
        categories c
    JOIN
        RecursiveCTE rcte ON c.parent_id = rcte.id
)

SELECT
    rcte.id,
    rcte.parent_id,
    rcte.name,
    rcte.level
FROM
    RecursiveCTE rcte;
```

This is a recursive Common Table Expression (CTE) in SQL that can be used to retrieve hierarchical data from a table. In this case, we are using it to retrieve all the categories and subcategories from a categories table, including the level of each category in the hierarchy.

The RecursiveCTE CTE is defined using the WITH clause. The first part of the CTE, the SELECT statement, defines the base case of the recursion. In this case, we are selecting all the categories that have a level of 1, which are the top-level categories.

The second part of the CTE, the UNION ALL statement, defines the recursive case. In this case, we are selecting all the categories that have a parent_id that matches the id of a category in the RecursiveCTE CTE. We are also incrementing the level by 1 for each level of recursion.

The final SELECT statement selects the id, parent_id, name, and level columns from the RecursiveCTE CTE. This will return all the categories and subcategories in the hierarchy, along with their level in the hierarchy.

This code can be used to retrieve hierarchical data from any table in a database. It is a powerful tool that can be used to solve a variety of problems.