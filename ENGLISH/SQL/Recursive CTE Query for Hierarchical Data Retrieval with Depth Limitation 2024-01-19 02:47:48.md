```sql
WITH RecursiveCTE AS (
    SELECT
        id,
        parent_id,
        name,
        depth
    FROM
        categories
    WHERE
        depth = 0  -- Starting from the root categories
    UNION ALL
    SELECT
        c.id,
        c.parent_id,
        c.name,
        r.depth + 1
    FROM
        categories AS c
    JOIN
        RecursiveCTE AS r
    ON
        c.parent_id = r.id
)

SELECT
    r.id,
    r.parent_id,
    r.name,
    r.depth
FROM
    RecursiveCTE AS r
WHERE
    r.depth <= 3;  -- Limiting the depth to three levels;
```

This SQL code demonstrates a complex and comprehensive recursive query using Common Table Expression (CTE) to retrieve hierarchical data with a maximum depth of three levels. The RecursiveCTE CTE recursively traverses the categories table to construct a hierarchical structure, where each row represents a category and its depth in the hierarchy.

The RecursiveCTE CTE starts by selecting categories with a depth of 0, which are the root categories. It then recursively combines the results of this query with itself, joining each child category with its parent category and incrementing the depth by 1.

The final SELECT statement retrieves the desired data from the RecursiveCTE CTE, including the category ID, parent ID, name, and depth, and filters the results to include only categories with a depth of 3 or less.

This code is useful for complex hierarchical data structures, such as categories, organizational structures, family trees, and other scenarios where you need to extract data at specific levels or explore relationships across multiple levels.

For example, consider a database table called "categories" with columns like "id", "parent_id", and "name" representing a hierarchy of categories. To retrieve all categories and their names up to a maximum depth of three levels, you can use this code.

The results of this code would provide a hierarchical view of categories, displaying their parent-child relationships and the depth of each category in the hierarchy. This is helpful for tasks like generating nested menus, performing hierarchical analysis, or visualizing the structure of complex data.