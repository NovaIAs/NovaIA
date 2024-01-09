```d
import std.stdio, std.algorithm, std.math, std.traits;

const int MAX_N = 1000000; // Maximum size of the array

// Define a custom type for storing the points in the 2D plane
struct Point {
    real x, y;
    Point(real x = 0, real y = 0) : x(x), y(y) {}

    // Overload the + operator to enable vector addition
    Point operator+(const Point& other) const {
        return Point(x + other.x, y + other.y);
    }

    // Overload the - operator to enable vector subtraction
    Point operator-(const Point& other) const {
        return Point(x - other.x, y - other.y);
    }

    // Overload the * operator to enable scalar multiplication
    Point operator*(const real& scalar) const {
        return Point(x * scalar, y * scalar);
    }

    // Overload the / operator to enable scalar division
    Point operator/(const real& scalar) const {
        return Point(x / scalar, y / scalar);
    }

    // Overload the == operator to compare two points for equality
    bool operator==(const Point& other) const {
        return x == other.x && y == other.y;
    }

    // Overload the != operator to compare two points for inequality
    bool operator!=(const Point& other) const {
        return !(*this == other);
    }

    // Define a method to calculate the Euclidean distance between two points
    real distance(const Point& other) const {
        return std.sqrt((x - other.x) * (x - other.x) + (y - other.y) * (y - other.y));
    }

    // Define a method to calculate the angle between two points in radians
    real angle(const Point& other) const {
        return std.atan2(y - other.y, x - other.x);
    }
};

// Define a custom type for storing a line segment in the 2D plane
struct LineSegment {
    Point p1, p2; // Endpoints of the line segment
    LineSegment(const Point& p1, const Point& p2) : p1(p1), p2(p2) {}

    // Define a method to calculate the length of the line segment
    real length() const {
        return p1.distance(p2);
    }

    // Define a method to calculate the midpoint of the line segment
    Point midpoint() const {
        return (p1 + p2) / 2;
    }

    // Define a method to check if a point lies on the line segment
    bool contains(const Point& p) const {
        return (p1.distance(p) + p2.distance(p) == length());
    }

    // Define a method to check if two line segments intersect
    bool intersects(const LineSegment& other) const {
        // Check if the line segments are collinear
        if (p1.x == p2.x && other.p1.x == other.p2.x) {
            // Check if the line segments overlap
            return (std.min(p1.y, p2.y) <= std.max(other.p1.y, other.p2.y) &&
                   std.max(p1.y, p2.y) >= std.min(other.p1.y, other.p2.y));
        } else if (p1.y == p2.y && other.p1.y == other.p2.y) {
            // Check if the line segments overlap
            return (std.min(p1.x, p2.x) <= std.max(other.p1.x, other.p2.x) &&
                   std.max(p1.x, p2.x) >= std.min(other.p1.x, other.p2.x));
        } else {
            // Calculate the slopes of the two line segments
            real m1 = (p2.y - p1.y) / (p2.x - p1.x);
            real m2 = (other.p2.y - other.p1.y) / (other.p2.x - other.p1.x);

            // Check if the slopes are equal
            if (m1 == m2) {
                // Check if the line segments are collinear
                return (p1.x * m1 + p1.y == p2.x * m1 + p2.y &&
                       other.p1.x * m2 + other.p1.y == other.p2.x * m2 + other.p2.y);
            } else {
                // Calculate the point of intersection
                real x = (m1 * p1.x - m2 * other.p1.x + other.p1.y - p1.y) / (m1 - m2);
                real y = m1 * (x - p1.x) + p1.y;

                // Check if the point of intersection lies on both line segments
                return (contains(Point(x, y)) && other.contains(Point(x, y)));
            }
        }
    }
};

// Define a custom type for storing a polygon in the 2D plane
struct Polygon {
    std.vector!Point vertices; // Vertices of the polygon

    // Define a method to add a vertex to the polygon
    void addVertex(const Point& vertex) {
        vertices.push_back(vertex);
    }

    // Define a method to calculate the perimeter of the polygon
    real perimeter() const {
        real perimeter = 0;
        for (int i = 0; i < vertices.length - 1; i++) {
            perimeter += vertices[i].distance(vertices[i + 1]);
        }
        perimeter += vertices[vertices.length - 1].distance(vertices[0]);
        return perimeter;
    }

    // Define a method to calculate the area of the polygon
    real area() const {
        real area = 0;
        for (int i = 0; i < vertices.length - 1; i++) {
            area += (vertices[i].x * vertices[i + 1].y - vertices[i + 1].x * vertices[i].y);
        }
        area += (vertices[vertices.length - 1].x * vertices[0].y - vertices[0].x * vertices[vertices.length - 1].y);
        return std.abs(area) / 2;
    }

    // Define a method to check if a point lies inside the polygon
    bool contains(const Point& point) const {
        // Check if the point is on a vertex
        for (int i = 0; i < vertices.length; i++) {
            if (point == vertices[i]) {
                return true;
            }
        }

        // Count the number of intersections between a ray from the point to infinity and the polygon's edges
        int intersections = 0;
        for (int i = 0; i < vertices.length; i++) {
            LineSegment edge(vertices[i], vertices[(i + 1) % vertices.length]);
            LineSegment ray(point, Point(std.inf, point.y));
            if (edge.intersects(ray)) {
                intersections++;
            }
        }

        // If the number of intersections is odd, the point is inside the polygon
        return intersections % 2 == 1;
    }
};

// Define a function to generate a random point in the 2D plane
Point generateRandomPoint(real minX, real maxX, real minY, real maxY) {
    real x = std.random.float(minX, maxX);
    real y = std.random.float(minY, maxY);
    return Point(x, y);
}

// Define a function to generate a random polygon in the 2D plane
Polygon generateRandomPolygon(int numVertices, real minX, real maxX, real minY, real maxY) {
    Polygon polygon;
    for (int i = 0; i < numVertices; i++) {
        polygon.addVertex(generateRandomPoint(minX, maxX, minY, maxY));
    }
    return polygon;
}

// Define a function to test the polygon's properties
void testPolygonProperties(const Polygon& polygon) {
    std.stdio.writeln("Perimeter:", polygon.perimeter());
    std.stdio.writeln("Area:", polygon.area());

    // Generate a random point and check if it lies inside the polygon
    Point randomPoint = generateRandomPoint(-100, 100, -100, 100);
    std.stdio.writeln("Random point:", randomPoint);
    std.stdio.writeln("Point inside polygon:", polygon.