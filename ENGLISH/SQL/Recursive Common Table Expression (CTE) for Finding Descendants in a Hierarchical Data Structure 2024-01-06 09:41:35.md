```sql
-- Create a common table expression (CTE) called `RecursiveCTE` that recursively finds all the descendants of a given node in a hierarchical data structure.

WITH RecursiveCTE (id, name, parent_id, level) AS (
    -- Base case: Select the node itself as the starting point of the recursion.
    SELECT id, name, parent_id, 0 AS level
    FROM table_name
    WHERE id = @starting_node_id

    UNION ALL

    -- Recursive case: Select all the descendants of the current node.
    SELECT t.id, t.name, t.parent_id, rcte.level + 1
    FROM table_name AS t
    JOIN RecursiveCTE AS rcte ON t.parent_id = rcte.id
)

-- Select the id, name, and level of all the descendants of the given node.
SELECT id, name, level
FROM RecursiveCTE
ORDER BY level;
```

Explanation:

1. The `WITH` clause creates a common table expression (CTE) named `RecursiveCTE`.
2. The first subquery in the `WITH` clause (`SELECT id, name, parent_id, 0 AS level`) selects the node with the specified `@starting_node_id` as the starting point of the recursion. It sets the `level` column to 0 for this node.
3. The second subquery in the `WITH` clause (`SELECT t.id, t.name, t.parent_id, rcte.level + 1`) performs the recursive step. It selects all the descendants of the current node `t` by joining `t` with the `RecursiveCTE` CTE on the `parent_id` column. It increments the `level` column by 1 for each descendant.
4. The `UNION ALL` operator combines the results of the base case and the recursive case into a single result set.
5. The `SELECT` statement outside the `WITH` clause selects the `id`, `name`, and `level` columns from the `RecursiveCTE` CTE.
6. The `ORDER BY` clause sorts the results by the `level` column in ascending order.

This code can be used to find all the descendants of a given node in a hierarchical data structure, such as a file system or an organizational chart. It uses a recursive CTE to efficiently traverse the data structure and find all the descendants of the given node.