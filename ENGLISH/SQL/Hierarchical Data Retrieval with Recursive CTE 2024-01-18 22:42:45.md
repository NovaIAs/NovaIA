```sql
WITH RECURSIVE Hierarchy AS (
    -- Base Case: Select the root nodes (nodes with no parents)
    SELECT id, name, parent_id, 1 AS level
    FROM nodes
    WHERE parent_id IS NULL

    -- Recursive Case: Select child nodes and increment the level
    UNION ALL
    SELECT n.id, n.name, n.parent_id, h.level + 1
    FROM nodes n
    JOIN Hierarchy h ON n.parent_id = h.id
)

-- Select data from the Hierarchy CTE and add additional columns
SELECT h.id, h.name, h.parent_id, h.level,
       (SELECT GROUP_CONCAT(hn.name, ', ')
        FROM Hierarchy hn
        WHERE hn.level >= h.level
        ORDER BY hn.level) AS ancestors,
       (SELECT GROUP_CONCAT(hn.name, ', ')
        FROM Hierarchy hn
        WHERE hn.level <= h.level
        ORDER BY hn.level DESC) AS descendants
FROM Hierarchy h;
```

Explanation:

1. **Recursive CTE (Hierarchy):**
   - The WITH RECURSIVE statement creates a recursive common table expression (CTE) named Hierarchy.
   - The base case (first SELECT statement) retrieves the root nodes (nodes with no parents) and sets their level to 1.
   - The recursive case (second SELECT statement) selects child nodes and increments their level by 1.

2. **Level Column:**
   - The level column keeps track of the depth of each node in the hierarchy, with root nodes at level 1.

3. **Ancestors and Descendants:**
   - The ancestors column contains a comma-separated list of ancestor nodes (nodes above the current node in the hierarchy).
   - The descendants column contains a comma-separated list of descendant nodes (nodes below the current node in the hierarchy).

4. **Subqueries:**
   - The subqueries in the SELECT statement use GROUP_CONCAT to concatenate the names of ancestor or descendant nodes.
   - The ORDER BY clause ensures that the names are returned in order, from the root node to the current node (for ancestors) or vice versa (for descendants).

5. **Final SELECT:**
   - The final SELECT statement retrieves the id, name, parent_id, level, ancestors, and descendants columns for each node in the hierarchy.

This code efficiently retrieves hierarchical data from a nodes table, including information about each node's ancestors and descendants, and the level of each node in the hierarchy.