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
        depth = 0
    UNION ALL
    SELECT
        c.id,
        c.parent_id,
        c.name,
        rcte.depth + 1
    FROM
        categories c
    INNER JOIN
        RecursiveCTE rcte ON c.parent_id = rcte.id
)

SELECT
    rcte.id,
    rcte.parent_id,
    rcte.name,
    rcte.depth
FROM
    RecursiveCTE rcte;
```

Explanation:

The provided code is a recursive Common Table Expression (CTE) query in SQL that retrieves hierarchical data from a table named "categories." Here's a detailed explanation:

1. Base Query:

```sql
SELECT
    id,
    parent_id,
    name,
    depth
FROM
    categories
WHERE
    depth = 0;
```

- This initial query selects all categories from the "categories" table where the "depth" column is 0. The "depth" column typically represents the level of a category in a hierarchical structure, with 0 being the top level. This query fetches the top-level categories.

2. Recursive Union:

```sql
UNION ALL
SELECT
    c.id,
    c.parent_id,
    c.name,
    rcte.depth + 1
FROM
    categories c
INNER JOIN
    RecursiveCTE rcte ON c.parent_id = rcte.id;
```

- This recursive part of the query uses a self-join with the RecursiveCTE to fetch child categories. It selects categories (aliased as "c") and joins them with the RecursiveCTE based on the "parent_id" column.
- It calculates the "depth" of each child category by incrementing the "depth" value of the parent category from the RecursiveCTE.
- The query continues to recursively join and traverse the "categories" table, retrieving child categories for each parent category until there are no more child categories to fetch.

3. Final Result:

```sql
SELECT
    rcte.id,
    rcte.parent_id,
    rcte.name,
    rcte.depth
FROM
    RecursiveCTE rcte;
```

- Finally, the query selects the columns "id," "parent_id," "name," and "depth" from the RecursiveCTE. This retrieves all categories in the hierarchy, including their parent-child relationships and their depth levels.

This code allows you to retrieve hierarchical data from a table representing a category structure, such as a product catalog or an organizational chart, and organize it in a structured manner based on the depth of each category.