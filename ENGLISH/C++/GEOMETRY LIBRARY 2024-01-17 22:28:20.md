#include <iostream>
#include <cmath>
#include <vector>
#include <algorithm>

using namespace std;

class Point {
private:
    double x, y;

public:
    Point() : x(0), y(0) {}
    Point(double x, double y) : x(x), y(y) {}

    double getX() const { return x; }
    double getY() const { return y; }

    void setX(double x) { this->x = x; }
    void setY(double y) { this->y = y; }

    double distanceTo(const Point& other) const {
        double dx = x - other.x;
        double dy = y - other.y;
        return sqrt(dx * dx + dy * dy);
    }
};

class Line {
private:
    Point p1, p2;

public:
    Line() : p1(0, 0), p2(0, 0) {}
    Line(const Point& p1, const Point& p2) : p1(p1), p2(p2) {}

    Point getP1() const { return p1; }
    Point getP2() const { return p2; }

    void setP1(const Point& p1) { this->p1 = p1; }
    void setP2(const Point& p2) { this->p2 = p2; }

    double length() const {
        return p1.distanceTo(p2);
    }

    bool isParallelTo(const Line& other) const {
        double dx1 = p2.getX() - p1.getX();
        double dy1 = p2.getY() - p1.getY();
        double dx2 = other.p2.getX() - other.p1.getX();
        double dy2 = other.p2.getY() - other.p1.getY();
        return dx1 * dy2 == dx2 * dy1;
    }
};

class Circle {
private:
    Point center;
    double radius;

public:
    Circle() : center(0, 0), radius(0) {}
    Circle(const Point& center, double radius) : center(center), radius(radius) {}

    Point getCenter() const { return center; }
    double getRadius() const { return radius; }

    void setCenter(const Point& center) { this->center = center; }
    void setRadius(double radius) { this->radius = radius; }

    double circumference() const {
        return 2 * M_PI * radius;
    }

    double area() const {
        return M_PI * radius * radius;
    }

    bool containsPoint(const Point& point) const {
        return point.distanceTo(center) <= radius;
    }
};

class Rectangle {
private:
    Point topLeft, bottomRight;

public:
    Rectangle() : topLeft(0, 0), bottomRight(0, 0) {}
    Rectangle(const Point& topLeft, const Point& bottomRight) : topLeft(topLeft), bottomRight(bottomRight) {}

    Point getTopLeft() const { return topLeft; }
    Point getBottomRight() const { return bottomRight; }

    void setTopLeft(const Point& topLeft) { this->topLeft = topLeft; }
    void setBottomRight(const Point& bottomRight) { this->bottomRight = bottomRight; }

    double width() const {
        return bottomRight.getX() - topLeft.getX();
    }

    double height() const {
        return bottomRight.getY() - topLeft.getY();
    }

    double area() const {
        return width() * height();
    }

    bool containsPoint(const Point& point) const {
        return point.getX() >= topLeft.getX() && point.getX() <= bottomRight.getX() &&
               point.getY() >= topLeft.getY() && point.getY() <= bottomRight.getY();
    }
};

class Triangle {
private:
    Point p1, p2, p3;

public:
    Triangle() : p1(0, 0), p2(0, 0), p3(0, 0) {}
    Triangle(const Point& p1, const Point& p2, const Point& p3) : p1(p1), p2(p2), p3(p3) {}

    Point getP1() const { return p1; }
    Point getP2() const { return p2; }
    Point getP3() const { return p3; }

    void setP1(const Point& p1) { this->p1 = p1; }
    void setP2(const Point& p2) { this->p2 = p2; }
    void setP3(const Point& p3) { this->p3 = p3; }

    double semiperimeter() const {
        double a = p1.distanceTo(p2);
        double b = p2.distanceTo(p3);
        double c = p3.distanceTo(p1);
        return (a + b + c) / 2.0;
    }

    double area() const {
        double s = semiperimeter();
        double a = p1.distanceTo(p2);
        double b = p2.distanceTo(p3);
        double c = p3.distanceTo(p1);
        return sqrt(s * (s - a) * (s - b) * (s - c));
    }

    bool isRightTriangle() const {
        double a = p1.distanceTo(p2);
        double b = p2.distanceTo(p3);
        double c = p3.distanceTo(p1);
        return a * a + b * b == c * c || a * a + c * c == b * b || b * b + c * c == a * a;
    }
};

int main() {
    // Create some objects

    Point p1(1, 2);
    Point p2(3, 4);
    Point p3(5, 6);

    Line l1(p1, p2);
    Line l2(p2, p3);

    Circle c1(p1, 2);
    Circle c2(p2, 3);

    Rectangle r1(p1, p2);
    Rectangle r2(p2, p3);

    Triangle t1(p1, p2, p3);
    Triangle t2(p2, p3, p1);

    // Print the objects

    cout << "Point p1: (" << p1.getX() << ", " << p1.getY() << ")" << endl;
    cout << "Point p2: (" << p2.getX() << ", " << p2.getY() << ")" << endl;
    cout << "Point p3: (" << p3.getX() << ", " << p3.getY() << ")" << endl;

    cout << "Line l1: ";
    cout << "(" << l1.getP1().getX() << ", " << l1.getP1().getY() << ") to ";
    cout << "(" << l1.getP2().getX() << ", " << l1.getP2().getY() << ")" << endl;

    cout << "Line l2: ";
    cout << "(" << l2.getP1().getX() << ", " << l2.getP1().getY() << ") to ";
    cout << "(" << l2.getP2().getX() << ", " << l2.getP2().getY() << ")" << endl;

    cout << "Circle c1: center at (" << c1.getCenter().getX() << ", " << c1.getCenter().getY() << "), radius = " << c1.getRadius() << endl;

    cout << "Circle c2: center at (" << c2.getCenter().getX() << ", " << c2.getCenter().getY() << "), radius = " << c2.getRadius() << endl;

    cout << "Rectangle r1: top left at (" << r1.getTopLeft().getX() << ", " << r1.getTopLeft().getY() << "), bottom right at (" << r1.getBottomRight().getX() << ", " << r1.getBottomRight().getY() << ")" << endl;

    cout << "Rectangle r2: top left at (" << r2.getTopLeft().getX() << ", " << r2.getTopLeft().getY() << "), bottom right at (" << r2.getBottomRight().getX() << ", " << r2.getBottomRight().getY() << ")" << endl;

    cout << "Triangle t1: vertices at (" << t1.getP1().getX() << ", " << t1.getP1().getY() << "), (" << t1.getP2().getX() << ", " << t1.getP2().getY() << "), (" << t1.getP3().getX() << ", " << t1.getP3().getY() << ")" << endl;

    cout << "Triangle t2: vertices at (" << t2.getP1().getX() << ", " << t2.getP1().getY() << "), (" << t2.getP2().getX() << ", " << t2.getP2().getY() << "), (" << t2.getP3().getX() << ", " << t2.getP3().getY() << ")" << endl;

    // Check some properties

    cout << "Distance between p1 and p2: " << p1.distanceTo(p2) << endl;
    cout << "Length of l1: " << l1.length() << endl;
    cout << "Is l1 parallel to l2? " << (l1.isParallelTo(l2) ? "Yes" : "No") << endl;
    cout << "Area of c1: " << c1.area() << endl;
    cout << "Does c1 contain p2? " << (c1.containsPoint(p2) ? "Yes" : "No") << endl;
    cout << "Area of r1: " << r1.area() << endl;
    cout << "Does r1 contain p3? " << (r1.containsPoint(p3) ? "Yes" : "No") << endl;
    cout << "Area of t1: " << t1.area() << endl;
    cout << "Is t1 a right triangle? " << (t1.isRightTriangle() ? "Yes" : "No") << endl;

    return 0;
}