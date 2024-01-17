```sql
WITH RecursiveCTE AS (
    -- Base case: start with the root node
    SELECT id, name, NULL AS parent_id
    FROM nodes
    WHERE id = 1
    
    UNION ALL
    
    -- Recursive case: find all children of the current node
    SELECT n.id, n.name, p.id AS parent_id
    FROM nodes AS n
    JOIN RecursiveCTE AS p ON n.parent_id = p.id
)

-- Select the id, name, and path from the recursive CTE
SELECT id, name, 
    (
        -- Use a window function to concatenate the path segments
        SELECT GROUP_CONCAT(name, ' > ')
        FROM RecursiveCTE
        WHERE id BETWEEN RecursiveCTE.parent_id AND RecursiveCTE.id
        ORDER BY id
    ) AS path
FROM RecursiveCTE;
```

Explanation:

1. The RecursiveCTE common table expression (CTE) is defined using a recursive query.

2. The base case of the recursion is to select the root node (with id = 1) and its name, with a parent_id of NULL.

3. The recursive case of the recursion finds all children of the current node by joining the nodes table with the RecursiveCTE on the parent_id column.

4. The SELECT statement then uses the RecursiveCTE to select the id, name, and path of each node.

5. The path is calculated using a window function (GROUP_CONCAT) to concatenate the names of all nodes in the path from the root node to the current node.

6. The ORDER BY clause ensures that the nodes are ordered by their id, so that the path is displayed in the correct order.