```sql
WITH RecursiveCTE AS (
    SELECT
        *,
        ROW_NUMBER() OVER (PARTITION BY ParentID ORDER BY ID) AS Level
    FROM
        Categories
    WHERE
        ParentID IS NULL
    UNION ALL
    SELECT
        *,
        ROW_NUMBER() OVER (PARTITION BY ParentID ORDER BY ID) AS Level
    FROM
        Categories
    JOIN
        RecursiveCTE ON Categories.ParentID = RecursiveCTE.ID
)

SELECT
    ID,
    Name,
    Level
FROM
    RecursiveCTE
ORDER BY
    Level,
    ID;
```

This SQL code performs a recursive query on a table called "Categories" to retrieve all categories and their parent-child relationships in a hierarchical format. Here's how it works:

1. **RecursiveCTE:**
   - The RecursiveCTE (Recursive Common Table Expression) is created using the WITH clause.
   - It starts by selecting all the categories where the ParentID column is NULL, which represents the top-level categories.
   - It then uses the UNION ALL operator to recursively join the Categories table to itself based on the ParentID column.
   - The Level column is added to keep track of the level of each category in the hierarchy.

2. **Recursive Query:**
   - The recursive query is executed using the RecursiveCTE as the main table.
   - It retrieves all the columns from the RecursiveCTE, including the ID, Name, and Level columns.

3. **Ordering:**
   - The results are ordered first by the Level column in ascending order, and then by the ID column in ascending order.

This code efficiently retrieves all categories and their hierarchical relationships in a single query. It can be useful for tasks such as generating category navigation menus, creating hierarchical reports, or performing complex data analysis.