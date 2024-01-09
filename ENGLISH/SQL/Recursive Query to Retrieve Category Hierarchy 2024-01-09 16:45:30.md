```sql
WITH RecursiveCTE AS (
    SELECT
        id,
        parent_id,
        name,
        level
    FROM
        Categories
    WHERE
        level = 1
    UNION ALL
    SELECT
        c.id,
        c.parent_id,
        c.name,
        rcte.level + 1
    FROM
        Categories c
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

Explanation:

* The RecursiveCTE Common Table Expression (CTE) is used to implement a recursive query that traverses the hierarchy of categories.
* The base case of the recursion is to select all categories at level 1 (i.e., top-level categories).
* The recursive case of the recursion is to select all categories that have a parent_id equal to the id of a category in the RecursiveCTE.
* The level column is used to keep track of the depth of each category in the hierarchy.
* The final SELECT statement retrieves the id, parent_id, name, and level of all categories in the hierarchy.