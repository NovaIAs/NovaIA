```sql
WITH RecursiveCTE AS (
    SELECT
        nodeId,
        parentNodeId,
        nodeData,
        CAST(0 AS BIGNUMERIC) AS distanceFromRoot
    FROM
        nodes
    WHERE
        parentNodeId IS NULL

    UNION ALL

    SELECT
        n.nodeId,
        n.parentNodeId,
        n.nodeData,
        r.distanceFromRoot + CAST(1 AS BIGNUMERIC) AS distanceFromRoot
    FROM
        RecursiveCTE r
    JOIN
        nodes n ON r.nodeId = n.parentNodeId
)

SELECT
    nodeId,
    parentNodeId,
    nodeData,
    distanceFromRoot
FROM
    RecursiveCTE
ORDER BY
    distanceFromRoot ASC;
```

This code is a recursive common table expression (CTE) that calculates the distance from the root node for all nodes in a tree structure. The CTE is defined as follows:

* The first subquery selects all nodes that have a null parentNodeId, which are the root nodes of the tree. These nodes are assigned a distance from the root of 0.
* The second subquery recursively selects all other nodes in the tree, joining the RecursiveCTE to the nodes table on the parentNodeId column. The distance from the root for these nodes is calculated by adding 1 to the distance from the root of their parent nodes.

The final query selects all columns from the RecursiveCTE and orders the results by distance from the root in ascending order. This code can be used to perform various operations on a tree structure, such as finding the shortest path between two nodes or finding all nodes that are a certain distance from the root node.