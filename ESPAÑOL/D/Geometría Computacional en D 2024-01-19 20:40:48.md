```d
import std.stdio, std.algorithm;

struct Punto {
    real x, y;

    // Constructor
    this(real x, real y) {
        this.x = x;
        this.y = y;
    }

    // Sobrecarga del operador + para sumar dos puntos
    Punto operator+(Punto other) {
        return new Punto(x + other.x, y + other.y);
    }

    // Sobrecarga del operador - para restar dos puntos
    Punto operator-(Punto other) {
        return new Punto(x - other.x, y - other.y);
    }

    // Sobrecarga del operador * para multiplicar un punto por un escalar
    Punto operator*(real scalar) {
        return new Punto(x * scalar, y * scalar);
    }

    // Sobrecarga del operador / para dividir un punto por un escalar
    Punto operator/(real scalar) {
        return new Punto(x / scalar, y / scalar);
    }

    // Método para obtener la magnitud (longitud) del punto
    real magnitud() {
        return Math.sqrt(Math.pow(x, 2) + Math.pow(y, 2));
    }

    // Método para obtener el ángulo (en radianes) entre dos puntos
    real angulo(Punto other) {
        return Math.atan2(y - other.y, x - other.x);
    }

    // Método para obtener la distancia entre dos puntos
    real distancia(Punto other) {
        return (this - other).magnitud();
    }
}

// Clase para representar una línea
class Linea {
    Punto p1, p2;

    // Constructor
    this(Punto p1, Punto p2) {
        this.p1 = p1;
        this.p2 = p2;
    }

    // Método para obtener la longitud de la línea
    real longitud() {
        return p1.distancia(p2);
    }

    // Método para obtener la pendiente de la línea
    real pendiente() {
        return (p2.y - p1.y) / (p2.x - p1.x);
    }

    // Método para obtener la intersección con el eje Y
    real interseccionY() {
        return p1.y - pendiente() * p1.x;
    }

    // Método para obtener la intersección con el eje X
    real interseccionX() {
        return (interseccionY() / pendiente());
    }

    // Método para obtener el punto medio de la línea
    Punto puntoMedio() {
        return (p1 + p2) / 2;
    }

    // Método para determinar si un punto está en la línea
    bool contiene(Punto p) {
        return Math.abs(p.distancia(p1) + p.distancia(p2) - longitud()) < 1e-6;
    }

    // Método para determinar si dos líneas son paralelas
    bool esParalela(Linea other) {
        return Math.abs(pendiente() - other.pendiente()) < 1e-6;
    }

    // Método para determinar si dos líneas son perpendiculares
    bool esPerpendicular(Linea other) {
        return Math.abs(pendiente() * other.pendiente() + 1) < 1e-6;
    }

    // Método para obtener la intersección entre dos líneas
    Punto interseccion(Linea other) {
        if (esParalela(other)) {
            throw new Error("Las líneas son paralelas.");
        }

        real x = (other.interseccionY() - interseccionY()) / (pendiente() - other.pendiente());
        real y = pendiente() * x + interseccionY();

        return new Punto(x, y);
    }
}

// Clase para representar un polígono
class Poligono {
    mutable Punto[] puntos;

    // Constructor
    this(immutable Punto[] puntos) {
        this.puntos = puntos;
    }

    // Método para obtener el perímetro del polígono
    real perimetro() {
        real suma = 0;
        for (entero i = 0; i < puntos.length; i++) {
            suma += puntos[i].distancia(puntos[(i + 1) % puntos.length]);
        }

        return suma;
    }

    // Método para obtener el área del polígono
    real area() {
        real suma = 0;
        for (entero i = 0; i < puntos.length; i++) {
            suma += puntos[i].x * puntos[(i + 1) % puntos.length].y -
                   puntos[i].y * puntos[(i + 1) % puntos.length].x;
        }

        return Math.abs(suma) / 2;
    }

    // Método para determinar si un punto está dentro del polígono
    bool contiene(Punto p) {
        Linea rayo(Punto(0, 0), p);

        entero intersecciones = 0;
        for (entero i = 0; i < puntos.length; i++) {
            Linea lado(puntos[i], puntos[(i + 1) % puntos.length]);
            if (lado.contiene(p) || rayo.interseccion(lado).x > 0) {
                intersecciones++;
            }
        }

        return intersecciones % 2 == 1;
    }

    // Método para obtener el polígono convexo más pequeño que contiene al polígono actual
    Poligono convexo() {
        // Ordenar los puntos por su ángulo polar con respecto al origen
        Puntos[] ordenados = puntos.sort!((a, b) => a.angulo(Punto(0, 0)).compareTo(b.angulo(Punto(0, 0))));

        // Eliminar los puntos colineales
        Puntos[] colineales = []immutable Punto;
        for (enter