```d
import std.stdio, std.algorithm, std.range, std.typecons;

class Vector3 {
    real x, y, z;

    this(){
        this(0);
    }

    this(in real v) {
        this(v, v, v);
    }

    this(in real x, in real y, in real z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    real dotProduct(in Vector3 rhs) const pure nothrow {
        return x * rhs.x + y * rhs.y + z * rhs.z;
    }

    Vector3 crossProduct(in Vector3 rhs) const pure nothrow {
        return Vector3(
            y * rhs.z - z * rhs.y,
            z * rhs.x - x * rhs.z,
            x * rhs.y - y * rhs.x
        );
    }

    immutable real magnitude() const pure nothrow {
        return sqrt(dotProduct(*this));
    }

    @safe Vector3 normalize() pure nothrow {
        real mag = magnitude();
        if (mag != 0)
            return *this / mag;
        else
            return *this;
    }

    @safe Vector3 operator+(in Vector3 rhs) const pure nothrow {
        return Vector3(x + rhs.x, y + rhs.y, z + rhs.z);
    }

    @safe Vector3 operator-(in Vector3 rhs) const pure nothrow {
        return Vector3(x - rhs.x, y - rhs.y, z - rhs.z);
    }

    @safe Vector3 operator*(in real f) const pure nothrow {
        return Vector3(x * f, y * f, z * f);
    }

    @safe Vector3 operator/(in real f) const pure nothrow {
        return Vector3(x / f, y / f, z / f);
    }

    @safe Vector3 operator-() const pure nothrow {
        return Vector3(-x, -y, -z);
    }

    @safe Vector3& operator+=(in Vector3 rhs) pure nothrow {
        x += rhs.x;
        y += rhs.y;
        z += rhs.z;
        return *this;
    }

    @safe Vector3& operator-=(in Vector3 rhs) pure nothrow {
        x -= rhs.x;
        y -= rhs.y;
        z -= rhs.z;
        return *this;
    }

    @safe Vector3& operator*=(in real f) pure nothrow {
        x *= f;
        y *= f;
        z *= f;
        return *this;
    }

    @safe Vector3& operator/=(in real f) pure nothrow {
        x /= f;
        y /= f;
        z /= f;
        return *this;
    }

    void writeln() pure nothrow {
        writeln("Vector3: (", x, ", ", y, ", ", z, ")");
    }
}

void main() {
    immutable Vector3 a = Vector3(1, 2, 3);
    immutable Vector3 b = Vector3(4, 5, 6);

    writeln("a =", a);
    writeln("b =", b);
    writeln("a + b =", a + b);
    writeln("a - b =", a - b);
    writeln("a * 2 =", a * 2);
    writeln("a / 2 =", a / 2);
    writeln("-a =", -a);
    writeln("a.dotProduct(b) =", a.dotProduct(b));
    writeln("a.crossProduct(b) =", a.crossProduct(b));
    writeln("a.magnitude() =", a.magnitude());
    writeln("a.normalize() =", a.normalize());

    writeln("a += b =", a += b);
    writeln("a -= b =", a -= b);
    writeln("a *= 2 =", a *= 2);
    writeln("a /= 2 =", a /= 2);
}
```

This code defines a class called `Vector3` that represents a three-dimensional vector. The class includes methods for performing various operations on vectors, such as addition, subtraction, multiplication, division, dot product, cross product, magnitude, and normalization.

The `main` function creates two `Vector3` objects, `a` and `b`, and then uses the methods defined in the `Vector3` class to perform various operations on them. The results of these operations are printed to the console.

This code demonstrates the use of classes and methods in D, as well as the use of operator overloading to define custom operators for the `Vector3` class.