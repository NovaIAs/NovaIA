```groovy
// Define a class representing a node in a directed graph
class Node {
    // The name of the node
    String name

    // A list of the edges originating from this node
    List<Edge> outgoingEdges = []

    // A list of the edges terminating at this node
    List<Edge> incomingEdges = []

    // Add an outgoing edge to this node
    void addOutgoingEdge(Edge edge) {
        outgoingEdges.add(edge)
    }

    // Add an incoming edge to this node
    void addIncomingEdge(Edge edge) {
        incomingEdges.add(edge)
    }

    // Get the number of outgoing edges from this node
    int getOutgoingEdgeCount() {
        outgoingEdges.size()
    }

    // Get the number of incoming edges to this node
    int getIncomingEdgeCount() {
        incomingEdges.size()
    }

    // Get the list of outgoing edges from this node
    List<Edge> getOutgoingEdges() {
        outgoingEdges
    }

    // Get the list of incoming edges to this node
    List<Edge> getIncomingEdges() {
        incomingEdges
    }

    // Get the list of all nodes that are reachable from this node
    List<Node> getReachableNodes() {
        // Initialize the set of reachable nodes
        Set<Node> reachableNodes = new HashSet<>()

        // Perform a depth-first search starting from this node
        dfs(this, reachableNodes)

        // Return the set of reachable nodes
        reachableNodes
    }

    // Perform a depth-first search starting from this node
    void dfs(Node node, Set<Node> reachableNodes) {
        // Mark this node as visited
        reachableNodes.add(node)

        // Recursively visit all outgoing edges from this node
        for (Edge edge in node.outgoingEdges) {
            if (!reachableNodes.contains(edge.destination)) {
                dfs(edge.destination, reachableNodes)
            }
        }
    }

    // Get the shortest path from this node to a given destination node
    List<Edge> getShortestPathTo(Node destinationNode) {
        // Initialize the shortest path
        List<Edge> shortestPath = []

        // Initialize the queue of nodes to visit
        Queue<Node> queue = new LinkedList<>()

        // Initialize the map of visited nodes
        Map<Node, Node> visitedNodes = new HashMap<>()

        // Add this node to the queue
        queue.add(this)

        // Mark this node as visited
        visitedNodes.put(this, this)

        // While there are still nodes to visit
        while (!queue.isEmpty()) {
            // Get the next node to visit
            Node currentNode = queue.poll()

            // If the current node is the destination node, we have found the shortest path
            if (currentNode == destinationNode) {
                // Reconstruct the shortest path
                while (currentNode != this) {
                    shortestPath.add(visitedNodes.get(currentNode).outgoingEdges.find { it.destination == currentNode })
                    currentNode = visitedNodes.get(currentNode)
                }
                break
            }

            // Otherwise, visit all outgoing edges from the current node
            for (Edge edge in currentNode.outgoingEdges) {
                // If the destination node of the edge has not been visited
                if (!visitedNodes.containsKey(edge.destination)) {
                    // Add the destination node to the queue
                    queue.add(edge.destination)

                    // Mark the destination node as visited
                    visitedNodes.put(edge.destination, currentNode)
                }
            }
        }

        // Return the shortest path
        shortestPath
    }

    // Get the longest path from this node to a given destination node
    List<Edge> getLongestPathTo(Node destinationNode) {
        // Initialize the longest path
        List<Edge> longestPath = []

        // Initialize the queue of nodes to visit
        Queue<Node> queue = new LinkedList<>()

        // Initialize the map of visited nodes
        Map<Node, Node> visitedNodes = new HashMap<>()

        // Add this node to the queue
        queue.add(this)

        // Mark this node as visited
        visitedNodes.put(this, this)

        // While there are still nodes to visit
        while (!queue.isEmpty()) {
            // Get the next node to visit
            Node currentNode = queue.poll()

            // If the current node is the destination node, we have found the longest path
            if (currentNode == destinationNode) {
                // Reconstruct the longest path
                while (currentNode != this) {
                    longestPath.add(visitedNodes.get(currentNode).outgoingEdges.find { it.destination == currentNode })
                    currentNode = visitedNodes.get(currentNode)
                }
                break
            }

            // Otherwise, visit all outgoing edges from the current node
            for (Edge edge in currentNode.outgoingEdges) {
                // If the destination node of the edge has not been visited
                if (!visitedNodes.containsKey(edge.destination)) {
                    // Add the destination node to the queue
                    queue.add(edge.destination)

                    // Mark the destination node as visited
                    visitedNodes.put(edge.destination, currentNode)
                }
            }
        }

        // Return the longest path
        longestPath
    }

    // Get the number of paths from this node to a given destination node
    int getNumberOfPathsTo(Node destinationNode) {
        // Initialize the number of paths
        int numberOfPaths = 0

        // Initialize the queue of nodes to visit
        Queue<Node> queue = new LinkedList<>()

        // Initialize the map of visited nodes
        Map<Node, Integer> visitedNodes = new HashMap<>()

        // Add this node to the queue with a count of 1
        queue.add(this)
        visitedNodes.put(this, 1)

        // While there are still nodes to visit
        while (!queue.isEmpty()) {
            // Get the next node to visit
            Node currentNode = queue.poll()

            // If the current node is the destination node, we have found a path
            if (currentNode == destinationNode) {
                // Increment the