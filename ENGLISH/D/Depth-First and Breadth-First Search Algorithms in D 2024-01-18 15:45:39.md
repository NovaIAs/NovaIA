```d
// Sample self-referential struct

struct Node : AssocArray!T, AssocArray!2 {
    T value;
    Node*(Node) child;
    Node* next;

    override init() {
        value = null;
        child = null;
        next = null;
    }

    init(T value) {
        this.value = value;
        this.child = null;
        this.next = null;
    }
}

class Graph {
private:
    T[] vertices;
    Node(T)[] nodes;

public:
    Graph() {
        vertices = new T[];
        nodes = new Node(T)[];
    }

    void addVertex(T vertex) {
        vertices ~= vertex;

        // Initialize the corresponding entry in "nodes" to the new vertex
        nodes ~= new Node(vertex);
    }

    void addEdge(T source, T destination) {
        // Check if both vertices exist in the graph
        int sourceIndex = vertices.indexOf(source);
        int destinationIndex = vertices.indexOf(destination);

        if (sourceIndex == -1 || destinationIndex == -1) {
            throw new IllegalArgumentException("Invalid vertex");
        }

        // Create a new node for the edge and add it to the adjacency list of the source vertex
        Node(T) edgeNode = new Node(destination);
        nodes[sourceIndex].next = edgeNode;

        // Add the source vertex to the adjacency list of the destination vertex
        nodes[destinationIndex].child ~= source;
    }

    bool hasPathDFS(T start, T end) {
        // Check if both vertices exist in the graph
        int startIndex = vertices.indexOf(start);
        int endIndex = vertices.indexOf(end);

        if (startIndex == -1 || endIndex == -1) {
            return false;
        }

        // Create a stack to keep track of the vertices we have visited
        Stack stack = new Stack(vertices.length);

        // Create a set to keep track of the vertices we have already visited
        HashSet visited = new HashSet;

        // Push the start vertex onto the stack and mark it as visited
        stack.push(startIndex);
        visited.insert(startIndex);

        // While the stack is not empty, keep exploring the graph
        while (!stack.empty) {
            // Pop the top element from the stack
            int currentVertex = stack.pop;

            // If the current vertex is the destination, we have found a path
            if (currentVertex == endIndex) {
                return true;
            }

            // Get the adjacency list of the current vertex
            Node(T)[] neighbors = nodes[currentVertex].next;

            // Iterate over the neighbors of the current vertex
            for (int i = 0; i < neighbors.length; i++) {
                // Check if the neighbor has been visited
                if (!visited.contains(neighbors[i].value)) {
                    // If not, push it onto the stack and mark it as visited
                    stack.push(neighbors[i].value);
                    visited.insert(neighbors[i].value);
                }
            }
        }

        // If we have explored the entire graph and not found the destination, there is no path
        return false;
    }

    bool hasPathBFS(T start, T end) {
        // Check if both vertices exist in the graph
        int startIndex = vertices.indexOf(start);
        int endIndex = vertices.indexOf(end);

        if (startIndex == -1 || endIndex == -1) {
            return false;
        }

        // Create a queue to keep track of the vertices we need to explore
        Queue queue = new Queue(vertices.length);

        // Create a set to keep track of the visited vertices
        HashSet visited = new HashSet;

        // Enqueue the start vertex and mark it as visited
        queue.enqueue(startIndex);
        visited.insert(startIndex);

        // While the queue is not empty, keep exploring the graph
        while (!queue.empty) {
            // Dequeue the front element from the queue
            int currentVertex = queue.dequeue;

            // If the current vertex is the destination, we have found a path
            if (currentVertex == endIndex) {
                return true;
            }

            // Get the adjacency list of the current vertex
            Node(T)[] neighbors = nodes[currentVertex].next;

            // Iterate over the neighbors of the current vertex
            for (int i = 0; i < neighbors.length; i++) {
                // Check if the neighbor has been visited
                if (!visited.