```d
import std.stdio, std.algorithm, std.typecons, std.range, std.array;

struct Punto {
    float x, y;

    Punto(float x0, float y0) {
        this.x = x0;
        this.y = y0;
    }

    Punto() {
        this.x = 0.0;
        this.y = 0.0;
    }

    bool operator>(Punto other) const pure {
        return (this.x > other.x && this.y > other.y);
    }

    bool operator<(Punto other) const pure {
        return (this.x < other.x && this.y < other.y);
    }

    bool operator==(Punto other) const pure {
        return (this.x == other.x && this.y == other.y);
    }

    Punto operator+(Punto other) const pure {
        return Punto(this.x + other.x, this.y + other.y);
    }

    Punto operator-(Punto other) const pure {
        return Punto(this.x - other.x, this.y - other.y);
    }

    float operator*(Punto other) const pure {
        return (this.x * other.y - this.y * other.x);
    }

    float magnitud() const pure {
        return sqrt(this.x * this.x + this.y * this.y);
    }

    string to!string() const pure {
        return "(" ~ this.x ~ ", " ~ this.y ~ ")";
    }
}

struct Circulo {
    Punto centro;
    float radio;

    Circulo(Punto c, float r) {
        this.centro = c;
        this.radio = r;
    }

    bool contiene(Punto p) const pure {
        return (p - this.centro).magnitud() <= this.radio;
    }

    string to!string() const pure {
        return "Circulo {centro: " ~ this.centro.to!string() ~ ", radio: " ~ this.radio ~ "}";
    }
}

struct Linea {
    Punto a, b;

    Linea(Punto a0, Punto b0) {
        this.a = a0;
        this.b = b0;
    }

    bool contiene(Punto p) const pure {
        return (p - this.a)*(p - this.b) == 0;
    }

    string to!string() const pure {
        return "Linea {a: " ~ this.a.to!string() ~ ", b: " ~ this.b.to!string() ~ "}";
    }
}

struct Rectangulo {
    Punto a, b, c, d;

    Rectangulo(Punto a0, Punto b0, Punto c0, Punto d0) {
        this.a = a0;
        this.b = b0;
        this.c = c0;
        this.d = d0;
    }

    bool contiene(Punto p) const pure {
        return (p >= this.a && p <= this.b && p >= this.c && p <= this.d);
    }

    string to!string() const pure {
        return "Rectangulo {a: " ~ this.a.to!string() ~ ", b: " ~ this