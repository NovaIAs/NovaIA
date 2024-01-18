```sql
-- Recursive CTE to calculate the transitive closure of a directed graph

WITH RECURSIVE TransitiveClosure (start_node, end_node) AS (
    SELECT start_node, end_node
    FROM EdgeTable  -- Table containing the edges of the graph
    UNION ALL
    SELECT TransitiveClosure.start_node, EdgeTable.end_node
    FROM TransitiveClosure
    INNER JOIN EdgeTable ON TransitiveClosure.end_node = EdgeTable.start_node
)

-- Select all pairs of nodes that are connected in the transitive closure
SELECT DISTINCT start_node, end_node
FROM TransitiveClosure;


-- Complex SQL query to find the shortest path between two nodes in a weighted directed graph

WITH RECURSIVE PathFinder (start_node, end_node, path, distance) AS (
    SELECT start_node, end_node, ARRAY[start_node], 0
    FROM EdgeTable  -- Table containing the edges of the graph
    WHERE start_node = @start_node  -- Starting node of the path
    UNION ALL
    SELECT PathFinder.start_node, EdgeTable.end_node, PathFinder.path || EdgeTable.end_node, PathFinder.distance + EdgeTable.weight
    FROM PathFinder
    INNER JOIN EdgeTable ON PathFinder.end_node = EdgeTable.start_node
    WHERE EdgeTable.end_node NOT IN (SELECT node FROM PathFinder UNNEST path AS node)  -- Avoid cycles
)

-- Select the shortest path from the start node to the end node
SELECT path, distance
FROM PathFinder
WHERE start_node = @start_node AND end_node = @end_node
ORDER BY distance ASC
LIMIT 1;


-- Complex SQL query to find all paths between two nodes in a directed graph

WITH RECURSIVE PathFinder (start_node, end_node, path) AS (
    SELECT start_node, end_node, ARRAY[start_node]
    FROM EdgeTable  -- Table containing the edges of the graph
    WHERE start_node = @start_node  -- Starting node of the path
    UNION ALL
    SELECT PathFinder.start_node, EdgeTable.end_node, PathFinder.path || EdgeTable.end_node
    FROM PathFinder
    INNER JOIN EdgeTable ON PathFinder.end_node = EdgeTable.start_node
    WHERE EdgeTable.end_node NOT IN (SELECT node FROM PathFinder UNNEST path AS node)  -- Avoid cycles
)

-- Select all paths from the start node to the end node
SELECT DISTINCT path
FROM PathFinder
WHERE start_node = @start_node AND end_node = @end_node;


-- Complex SQL query to find all cycles in a directed graph

WITH RECURSIVE CycleFinder (start_node, end_node, path) AS (
    SELECT start_node, end_node, ARRAY[start_node]
    FROM EdgeTable  -- Table containing the edges of the graph
    WHERE start_node = end_node  -- Starting and ending node of a cycle
    UNION ALL
    SELECT CycleFinder.start_node, EdgeTable.end_node, CycleFinder.path || EdgeTable.end_node
    FROM CycleFinder
    INNER JOIN EdgeTable ON CycleFinder.end_node = EdgeTable.start_node
    WHERE EdgeTable.end_node NOT IN (SELECT node FROM CycleFinder UNNEST path AS node)  -- Avoid cycles
)

-- Select all cycles in the graph
SELECT DISTINCT path
FROM CycleFinder;


-- Complex SQL query to find the connected components of a graph

WITH RECURSIVE ConnectedComponents (component, node) AS (
    SELECT 1, start_node  -- Start with a new component for each node
    FROM EdgeTable  -- Table containing the edges of the graph
    UNION ALL
    SELECT component, EdgeTable.end_node
    FROM ConnectedComponents
    INNER JOIN EdgeTable ON ConnectedComponents.node = EdgeTable.start_node
    WHERE EdgeTable.end_node NOT IN (SELECT node FROM ConnectedComponents)  -- Add nodes to the component
)

-- Select the distinct components and the nodes in each component
SELECT DISTINCT component, ARRAY_AGG(node) AS nodes_in_component
FROM ConnectedComponents
GROUP BY component;
```

**Explanation:**

These SQL codes are complex and perform advanced operations on graph data structures. They use recursive common table expressions (CTEs) to traverse and explore the relationships between nodes and edges in a graph.

The first code calculates the transitive closure of a directed graph, which is a relation that includes all the paths between any two nodes in the graph, direct or indirect.

The second code finds the shortest path between two nodes in a weighted directed graph, taking into account the weights associated with each edge. It uses a dynamic programming approach with a recursive CTE to efficiently find the shortest path.

The third code finds all paths between two nodes in a directed graph. It also uses a recursive CTE to explore all possible paths and returns distinct paths from the starting node to the ending node.

The fourth code finds all cycles in a directed graph. It uses a recursive CTE to identify cycles in the graph and returns distinct cycles.

The fifth code finds the connected components of an undirected graph. It uses a recursive CTE to traverse the graph and assign each node to a connected component. It then groups the nodes by component and returns the distinct components and the nodes in each component.

These SQL codes demonstrate advanced graph traversal and analysis techniques, and they can be useful in various applications such as social network analysis, routing algorithms, and finding patterns in data represented as graphs.