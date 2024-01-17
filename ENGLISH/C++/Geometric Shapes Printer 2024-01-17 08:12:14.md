```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <string>
#include <cstring>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <ctime>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <utility>
#include <functional>
#include <bitset>

using namespace std;

#define ll long long
#define ld long double
#define ull unsigned long long
#define endl "\n"

struct point {
    ld x, y;
    point() {}
    point(ld _x, ld _y) : x(_x), y(_y) {}
    friend ostream& operator<<(ostream& os, const point& p) {
        return os << "(" << p.x << ", " << p.y << ")";
    }
};

struct line {
    ld a, b, c;
    line() {}
    line(ld _a, ld _b, ld _c) : a(_a), b(_b), c(_c) {}
    friend ostream& operator<<(ostream& os, const line& l) {
        return os << l.a << "x + " << l.b << "y + " << l.c << " = 0";
    }
};

struct circle {
    point center;
    ld radius;
    circle() {}
    circle(point _center, ld _radius) : center(_center), radius(_radius) {}
    friend ostream& operator<<(ostream& os, const circle& c) {
        return os << "Center: " << c.center << ", Radius: " << c.radius;
    }
};

struct triangle {
    point a, b, c;
    triangle() {}
    triangle(point _a, point _b, point _c) : a(_a), b(_b), c(_c) {}
    friend ostream& operator<<(ostream& os, const triangle& t) {
        return os << "Vertices: " << t.a << ", " << t.b << ", " << t.c;
    }
};

struct segment {
    point a, b;
    segment() {}
    segment(point _a, point _b) : a(_a), b(_b) {}
    friend ostream& operator<<(ostream& os, const segment& s) {
        return os << "Endpoints: " << s.a << ", " << s.b;
    }
};

struct rectangle {
    point a, b, c, d;
    rectangle() {}
    rectangle(point _a, point _b, point _c, point _d) : a(_a), b(_b), c(_c), d(_d) {}
    friend ostream& operator<<(ostream& os, const rectangle& r) {
        return os << "Vertices: " << r.a << ", " << r.b << ", " << r.c << ", " << r.d;
    }
};

struct polygon {
    vector<point> points;
    polygon() {}
    polygon(const vector<point>& _points) : points(_points) {}
    friend ostream& operator<<(ostream& os, const polygon& p) {
        os << "Vertices: ";
        for (const point& pt : p.points) {
            os << pt << ", ";
        }
        return os;
    }
};

int main() {
    // Print some basic geometric shapes
    cout << "Point: " << point(1.0, 2.0) << endl;
    cout << "Line: " << line(1.0, -2.0, 3.0) << endl;
    cout << "Circle: " << circle(point(0.0, 0.0), 5.0) << endl;
    cout << "Triangle: " << triangle(point(0.0, 0.0), point(1.0, 0.0), point(0.0, 1.0)) << endl;
    cout << "Segment: " << segment(point(0.0, 0.0), point(1.0, 1.0)) << endl;
    cout << "Rectangle: " << rectangle(point(0.0, 0.0), point(1.0, 0.0), point(1.0, 1.0), point(0.0, 1.0)) << endl;
    cout << "Polygon: " << polygon({point(0.0, 0.0), point(1.0, 0.0), point(1.0, 1.0), point(0.0, 1.0)}) << endl;

    return 0;
}
```