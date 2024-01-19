```sql
WITH RecursiveCTE AS (
    -- Base case: Select the root nodes of the tree.
    SELECT
        NodeID,
        ParentNodeID,
        NodeValue,
        1 AS Level
    FROM
        Nodes
    WHERE
        ParentNodeID IS NULL

    UNION ALL

    -- Recursive case: Select the children of the nodes in the current level.
    SELECT
        Nodes.NodeID,
        Nodes.ParentNodeID,
        Nodes.NodeValue,
        RecursiveCTE.Level + 1 AS Level
    FROM
        Nodes
    JOIN
        RecursiveCTE ON Nodes.ParentNodeID = RecursiveCTE.NodeID
)

-- Select the nodes that are at the lowest level of the tree.
SELECT
    NodeID,
    ParentNodeID,
    NodeValue
FROM
    RecursiveCTE
WHERE
    Level = (SELECT MAX(Level) FROM RecursiveCTE);
```

Explanation:

1. RecursiveCTE: We use a recursive Common Table Expression (CTE) called RecursiveCTE to perform a recursive query on the Nodes table.

2. Base Case: The base case of the CTE selects the root nodes of the tree. It retrieves the rows where the ParentNodeID column is NULL. These are the nodes that have no parent and are at the top of the tree.

3. Recursive Case: The recursive case of the CTE selects the children of the nodes in the current level. It joins the Nodes table with the RecursiveCTE on the ParentNodeID column and selects the child nodes. The Level column is incremented by 1 for each level of recursion.

4. Selecting the Lowest Level Nodes: After the recursive query is complete, we select the nodes that are at the lowest level of the tree. We do this by finding the maximum level from the RecursiveCTE and then selecting the nodes with that level.

Overall, this code performs a hierarchical query on a tree data structure in the Nodes table. It recursively traverses the tree starting from the root nodes and selects the nodes at the lowest level. This code is useful for various scenarios, such as finding the leaf nodes in a tree or computing the depth of each node.