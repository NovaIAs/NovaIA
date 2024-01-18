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
        parent_id IS NULL

    UNION ALL

    SELECT
        c.id,
        c.parent_id,
        c.name,
        rcte.level + 1 AS level
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
    RecursiveCTE rcte
ORDER BY
    rcte.level, rcte.name;
```

Explanation:

1. The RecursiveCTE Common Table Expression (CTE) is defined as a recursive query that starts with selecting categories that have no parent (parent_id is NULL). This forms the root of the hierarchical structure.

2. The UNION ALL operator is used to combine the initial result set (root categories) with the recursive part of the query.

3. The recursive part of the query (the second SELECT statement) joins the categories table (c) with the RecursiveCTE (rcte) using the parent_id column. This allows us to traverse down the category hierarchy.

4. The level column is used to keep track of the depth of each category in the hierarchy, starting from level 1 for root categories.

5. The final SELECT statement retrieves the id, parent_id, name, and level columns from the RecursiveCTE and orders the results by level (hierarchical depth) and then by name.

This query retrieves all categories and their hierarchical structure, with each level indented to visually represent the parent-child relationships.