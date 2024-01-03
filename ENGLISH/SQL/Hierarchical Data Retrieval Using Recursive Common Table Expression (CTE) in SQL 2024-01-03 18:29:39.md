```sql
WITH RecursiveCTE AS (
    SELECT
        *,
        ROW_NUMBER() OVER (PARTITION BY parent_id ORDER BY node_id) AS level
    FROM
        nodes
    WHERE
        parent_id IS NULL
    UNION ALL
    SELECT
        n.*,
        rc.level + 1
    FROM
        nodes AS n
    JOIN
        RecursiveCTE AS rc ON n.parent_id = rc.node_id
)
SELECT
    node_id,
    node_name,
    level
FROM
    RecursiveCTE
ORDER BY
    level,
    node_id;
```

Explanation:

1. `RecursiveCTE`: A recursive Common Table Expression (CTE) is created to traverse the hierarchical structure of the nodes table.
2. The first part of the CTE (`SELECT *, ROW_NUMBER() OVER ...`) assigns a level number to each node, starting from the root nodes (parent_id IS NULL).
3. The second part of the CTE (`UNION ALL SELECT ...`) recursively joins the CTE with itself to include child nodes at each level.
4. The main `SELECT` statement retrieves the node_id, node_name, and level for all nodes in the hierarchy.
5. The `ORDER BY` clause sorts the results first by level (depth in the hierarchy) and then by node_id (to ensure a consistent order within each level).

This query efficiently traverses and extracts data from a hierarchical structure represented in the nodes table, allowing you to explore complex relationships within the data.