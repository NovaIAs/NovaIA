```d
import std.stdio, std.algorithm;

enum Color {
    Red,
    Green,
    Blue
}

struct Point {
    int x, y;

    static Point origin = Point(0, 0);

    Point() pure nothrow {}
    Point(int x, int y) pure nothrow : this.x = x, this.y = y {}

    immutable Point operator+(const Point& other) const pure nothrow {
        return Point(x + other.x, y + other.y);
    }
    immutable Point operator-(const Point& other) const pure nothrow {
        return Point(x - other.x, y - other.y);
    }
    immutable Point operator*(const int& scalar) const pure nothrow {
        return Point(x * scalar, y * scalar);
    }
    immutable Point operator/(const int& scalar) const pure nothrow {
        return Point(x / scalar, y / scalar);
    }

    int magnitude() const pure nothrow {
        return sqrt(x*x + y*y);
    }

    immutable Point normalize() const pure nothrow {
        return this / magnitude();
    }

    immutable bool operator==(const Point& other) const pure nothrow {
        return x == other.x && y == other.y;
    }

    immutable bool operator<(const Point& other) const pure nothrow {
        return x < other.x || (x == other.x && y < other.y);
    }

    immutable void print() const pure nothrow {
        writefln("%d %d", x, y);
    }
}

immutable struct Line {
    const Point& a, b;

    static Line infinite(const Point& a, const Point& b) pure nothrow {
        return Line(a, b);
    }

    static Line horiz(const Point& a, const Point& b) pure nothrow {
        return Line(a, Point(b.x, a.y));
    }

    static Line vert(const Point& a, const Point& b) pure nothrow {
        return Line(a, Point(a.x, b.y));
    }

    Line(const Point& a, const Point& b) pure nothrow : this.a = a, this.b = b {}

    Point midpoint() const pure nothrow {
        return (a + b) / 2;
    }

    immutable double length() const pure nothrow {
        return (b - a).magnitude();
    }

    immutable bool isVertical() const pure nothrow {
        return a.x == b.x;
    }

    immutable bool isHorizontal() const pure nothrow {
        return a.y == b.y;
    }

    immutable bool isInfinite() const pure nothrow {
        return isVertical() || isHorizontal();
    }

    immutable bool contains(const Point& p) const pure nothrow {
        return (a.x <= p.x && p.x <= b.x) || (a.x >= p.x && p.x >= b.x) &&
               (a.y <= p.y && p.y <= b.y) || (a.y >= p.y && p.y >= b.y);
    }

    immutable bool intersects(const Line& other) const pure nothrow {
        if (isInfinite() || other.isInfinite()) {
            if (isInfinite() && other.isInfinite()) {
                return isVertical() && other.isVertical() ||
                       isHorizontal() && other.isHorizontal();
            } else if (isInfinite()) {
                return other.contains(a) || other.contains(b);
            } else {
                return contains(other.a) || contains(other.b);
            }
        }

        if (isVertical() && other.isHorizontal()) {
            return contains(other.a) || contains(other.b);
        } else if (isHorizontal() && other.isVertical()) {
            return other.contains(a) || other.contains(b);
        }

        int orientA = orientation(a, b, other.a);
        int orientB = orientation(a, b, other.b);
        int orientC = orientation(other.a, other.b, a);
        int orientD = orientation(other.a, other.b, b);

        return orientA != orientB && orientC != orientD;
    }

    immutable bool parallel(const Line& other) const pure nothrow {
        return isInfinite() && other.isInfinite() ||
               isVertical() && other.isVertical() ||
               isHorizontal() && other.isHorizontal();
    }

    immutable Point intersection(const Line& other) const pure nothrow {
        if (parallel(other)) {
            throw new Exception("Lines are parallel!");
        }

        double x1 = a.x, y1 = a.y, x2 = b.x, y2 = b.y;
        double x3 = other.a.x, y3 = other.a.y, x4 = other.b.x, y4 = other.b.y;

        double denom = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4);
        if (denom == 0) {
            throw new Exception("Lines are collinear!");
        }

        double t = ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / denom;
        return Line(a, b).pointAt(t);
    }

    immutable Point pointAt(double t) const pure nothrow {
        return a + (b - a) * t;
    }

    immutable int orientation(const Point& a, const Point& b, const Point& c) const pure nothrow {
        int val = (b.y - a.y) * (c.x - b.x) - (b.x - a.x) * (c.y - b.y);
        if (val == 0) {
            return 0;
        } else if (val > 0) {
            return 1;
        } else {
            return -1;
        }
    }

    immutable void print() const pure nothrow {
        writefln("%s -> %s", a.to!string, b.to!string);
    }
}

immutable struct Segment : Line {
    Segment(const Point& a, const Point& b) pure nothrow : this(Line.infinite(a, b)) {}
    Segment(const Line& line) pure nothrow : Line(line.a, line.b) {
        if (a.x > b.x || (a.x == b.x && a.y > b.y)) {
            this.a = b;
            this.b = a;
        }
    }

    bool intersects(const Segment& other) const pure nothrow {
        if (Line.intersects(other)) {
            return contains(other.a) || contains(other.b) ||
                   other.contains(a) || other.contains(b);
        }

        return false;
    }

    immutable void print() const pure nothrow {
        writefln("[%s -> %s]", a.to!string, b.to!string);
    }
};

immutable struct Ray : Line {
    Ray(const Point& a, const Point& b) pure nothrow : this(Line.infinite(a, b)) {}
    Ray(const Line& line) pure nothrow : Line(line.a, line.b) {}

    bool intersects(const Segment& other) const pure nothrow {
        if (Line.intersects(other)) {
            return contains(other.a) || contains(other.b);
        }

        return false;
    }

    immutable void print() const pure nothrow {
        writefln("%s -> ...", a.to!string);
    }
};

immutable struct Rect {
    const Point& a, b;

    Rect() pure nothrow {}
    Rect(const Point& a, const Point& b) pure nothrow : this.a = a, this.b = b {}

    immutable int width() const pure nothrow {
        return b.x - a.x;
    }

    immutable int height() const pure nothrow {
        return b.y - a.y;
    }

    immutable bool contains(const Point& p) const pure nothrow {
        return a.x <= p.x && p.x <= b.x &&
               a.y <= p.y && p.y <= b.y;
    }

    immutable bool intersects(const Rect& other) const pure nothrow {
        return a.x < other.b.x && b