```d
import std.stdio, std.range, std.algorithm, std.thread;

void main() {
    // Definir un tipo de dato complejo
    struct Complejo {
        real, imag: double;

        // Constructor
        this(real, imag) {
            this.real = real;
            this.imag = imag;
        }

        // Operadores sobrecargados
        Complejo operator+(Complejo otro) {
            return Complejo(real + otro.real, imag + otro.imag);
        }
        Complejo operator-(Complejo otro) {
            return Complejo(real - otro.real, imag - otro.imag);
        }
        Complejo operator*(Complejo otro) {
            return Complejo(real * otro.real - imag * otro.imag,
                           real * otro.imag + imag * otro.real);
        }
        Complejo operator/(Complejo otro) {
            double denominador = otro.real * otro.real + otro.imag * otro.imag;
            return Complejo((real * otro.real + imag * otro.imag) / denominador,
                           (imag * otro.real - real * otro.imag) / denominador);
        }

        // Método para mostrar el número complejo
        void mostrar() {
            writefln("%f + %fi", real, imag);
        }
    }

    // Crear algunos números complejos
    Complejo a = Complejo(3, 4);
    Complejo b = Complejo(5, 6);

    // Sumar y mostrar los números complejos
    Complejo suma = a + b;
    writefln("Suma:");
    suma.mostrar();

    // Restar y mostrar los números complejos
    Complejo resta = a - b;
    writefln("Resta:");
    resta.mostrar();

    // Multiplicar y mostrar los números complejos
    Complejo producto = a * b;
    writefln("Producto:");
    producto.mostrar();

    // Dividir y mostrar los números complejos
    Complejo division = a / b;
    writefln("División:");
    division.mostrar();

    // Definir una función que calcule la potencia de un número complejo
    Complejo potencia(Complejo c, int n) {
        if (n == 0) {
            return Complejo(1, 0);
        }
        if (n % 2 == 0) {
            Complejo cuadrado = potencia(c, n / 2);
            return cuadrado * cuadrado;
        } else {
            return c * potencia(c, n - 1);
        }
    }

    // Calcular y mostrar la potencia de un número complejo
    Complejo potencia_3 = potencia(a, 3);
    writefln("Potencia de 3:");
    potencia_3.mostrar();

    // Definir una función que calcule la raíz cuadrada de un número complejo
    Complejo raiz_cuadrada(Complejo c) {
        double r = sqrt(c.real * c.real + c.imag * c.imag);
        double theta = atan2(c.imag, c.real);
        return Complejo(r * cos(theta / 2), r * sin(theta / 2));
    }

    // Calcular y mostrar la raíz cuadrada de un número complejo
    Complejo raiz_cuadrada_a = raiz_cuadrada(a);
    writefln("Raíz cuadrada de a:");
    raiz_cuadrada_a.mostrar();

    // Definir una función que calcule el módulo de un número complejo
    double modulo(Complejo c) {
        return sqrt(c.real * c.real + c.imag * c.imag);
    }

    // Calcular y mostrar el módulo de un número complejo
    double modulo_a = modulo(a);
    writefln("Módulo de a:");
    writefln(modulo_a);

    // Definir una función que calcule el argumento de un número complejo
    double argumento(Complejo c) {
        return atan2(c.imag, c.real);
    }

    // Calcular y mostrar el argumento de un número complejo
    double argumento_a = argumento(a);
    writefln("Argumento de a:");
    writefln(argumento_a);

    // Definir una función que calcule el conjugado de un número complejo
    Complejo conjugado(Complejo c) {
        return Complejo(c.real, -c.imag);
    }

    // Calcular y mostrar el conjugado de un número complejo
    Complejo conjugado_a = conjugado(a);
    writefln("Conjugado de a:");
    conjugado_a.mostrar();

    // Definir una función que calcule la distancia entre dos números complejos
    double distancia(Complejo c1, Complejo c2) {
        double dx = c2.real - c1.real;
        double dy = c2.imag - c1.imag;
        return sqrt(dx * dx + dy * dy);
    }

    // Calcular y mostrar la distancia entre dos números complejos
    double distancia_ab = distancia(a, b);
    writefln("Distancia entre a y b:");
    writefln(distancia_ab);

    // Definir una función que calcule el producto escalar de dos números complejos
    double producto_escalar(Complejo c1, Complejo c2) {
        return c1.real * c2.real + c1.imag * c2.imag;
    }

    // Calcular y mostrar el producto escalar de dos números complejos
    double producto_escalar_ab = producto_escalar(a, b);
    writefln("Producto escalar de a y b:");
    writefln(producto_escalar_ab);

    // Definir una función que calcule el producto vectorial de dos números complejos
    double producto_vectorial(Complejo c1, Complejo c2) {
        return c1.real * c2.imag - c1.imag * c2.real;
    }

    // Calcular y mostrar el producto vectorial de dos números complejos
    double producto_vectorial_ab = producto_vectorial(a, b);
    writefln("Producto vectorial de a y b:");
    writefln(producto_vectorial_ab);

    // Definir una función que ordene una lista de números complejos
    void ordenar_complejos(in out Complejo[] complejos) {
        complejos.sort!{a, b -> a.real < b.real || (a.real == b.real && a.imag < b.imag) };
    }

    // Crear una lista de números complejos
    immutable Complejo[] complejos = [a, b, Complejo(2, 3), Complejo(4, 5), Complejo(1, 1)];

    // Ordenar la lista de números complejos
    ordenar_complejos!(complejos);

    // Mostrar la lista ordenada de números complejos
    writefln("Lista ordenada de números complejos:");
    for (Complejo complejo in complejos) {
        complejo.mostrar();
    }

    // Definir una función que calcule la media de una lista de números complejos
    Complejo media_complejos(in Complejo[] complejos) {
        double suma_real = 0, suma_imag = 0;
        for (Complejo complejo in complejos) {
            suma_real += complejo.real;
            suma_imag += complejo.imag;
        }
        return Complejo(suma_real / complejos.length, suma_imag / complejos.length);
    }

    // Calcular y mostrar la media de una lista de números complejos