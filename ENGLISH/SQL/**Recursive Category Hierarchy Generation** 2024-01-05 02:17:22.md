```sql
WITH RecursiveCategoryHierarchy AS (
    SELECT
        CategoryID,
        CategoryName,
        ParentCategoryID,
        CAST(CategoryID AS VARCHAR(255)) AS Path
    FROM
        Categories
    WHERE
        ParentCategoryID IS NULL

    UNION ALL

    SELECT
        c.CategoryID,
        c.CategoryName,
        c.ParentCategoryID,
        rch.Path || '/' || CAST(c.CategoryID AS VARCHAR(255))
    FROM
        Categories c
    JOIN
        RecursiveCategoryHierarchy rch ON c.ParentCategoryID = rch.CategoryID
)

SELECT DISTINCT
    rch.CategoryID,
    rch.CategoryName,
    rch.Path AS CategoryHierarchy
FROM
    RecursiveCategoryHierarchy rch
ORDER BY
    rch.Path;
```

Explanation:

This code is a recursive common table expression (CTE) that generates a hierarchical representation of categories in a database. It starts by selecting the top-level categories (those without a parent category) and their category ID, name, parent category ID, and a path that initially contains only the category ID.

The recursive part of the CTE then joins the categories table to the result of the previous query, selecting child categories and appending their category ID to the path of their parent category. This process continues until all categories have been processed and their paths have been generated.

The final result is a table that contains all categories, their names, and their hierarchical paths. The paths are represented as strings that separate category IDs with forward slashes (/). This allows for easy navigation of the category hierarchy.

To use this code, you can replace the Categories table name with the actual table name in your database. You can also modify the selection of columns in the final result to include additional information about the categories, such as descriptions or images.