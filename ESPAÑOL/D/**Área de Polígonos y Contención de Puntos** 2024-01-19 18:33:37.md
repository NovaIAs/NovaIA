```d
import std.stdio, std.algorithm, std.string, std.range, std.typecons;

struct Punto {
    float x, y;
    Punto(float x, float y) pure nothrow {
        this.x = x;
        this.y = y;
    }
}

struct Poligono {
    immutable Punto[] puntos;

    // Constructor
    Poligono(in Punto[] ps) pure nothrow {
        // Copiamos los puntos a un nuevo array para evitar modificar el array
        // original.
        puntos = Poligono.copiarPuntos(ps);
    }

    // Copia los puntos de un polígono a un nuevo array.
    static Punto[] copiarPuntos(in Punto[] ps) pure nothrow {
        auto puntosCopia = new Punto[ps.length];
        for (size_t i = 0; i < ps.length; i++) {
            puntosCopia[i] = ps[i];
        }
        return puntosCopia;
    }

    // Calcula el área del polígono.
    float calcularArea() const pure nothrow {
        // Si el polígono tiene menos de 3 puntos, el área es 0.
        if (puntos.length < 3) {
            return 0;
        }

        // Calculamos el área del polígono usando la fórmula de
        // Heron.
        float area = 0;
        for (size_t i = 0; i < puntos.length; i++) {
            auto p1 = puntos[i];
            auto p2 = puntos[(i + 1) % puntos.length];
            area += p1.x * p2.y - p2.x * p1.y;
        }
        area = abs(area) / 2;

        return area;
    }

    // Determina si el punto dado está dentro del polígono.
    bool puntoDentro(in Punto punto) const pure nothrow {
        // Si el polígono tiene menos de 3 puntos, no hay ningún punto
        // dentro de él.
        if (puntos.length < 3) {
            return false;
        }

        // Calculamos el número de intersecciones entre una línea horizontal
        // que pasa por el punto dado y los lados del polígono. Si el número
        // de intersecciones es impar, el punto está dentro del polígono.
        int intersecciones = 0;
        for (size_t i = 0; i < puntos.length; i++) {
            auto p1 = puntos[i];
            auto p2 = puntos[(i + 1) % puntos.length];

            if (p1.y <= punto.y && p2.y > punto.y) {
                intersecciones++;
            } else if (p2.y <= punto.y && p1.y > punto.y) {
                intersecciones--;
            }
        }

        return intersecciones % 2 != 0;
    }
}

// Función `main`
void main() {
    // Creamos un polígono de 5 vértices.
    auto vertices = [
        Punto(0, 0),
        Punto(1, 0),
        Punto(2, 1),
        Punto(1, 2),
        Punto(0, 1)
    ];
    auto poligono = Poligono(vertices);

    // Calculamos el área del polígono.
    float area = poligono.calcularArea();

    // Imprimimos el área del polígono y si el punto (0.5, 0.5) está
    // dentro del polígono.
    writeln("Área del polígono:", area);
    writeln("Punto (0.5, 0.5) dentro del polígono:", poligono.puntoDentro(Punto(0.5, 0.5)));
}
```

Explicación:

Este código crea una estructura `Punto` para representar puntos en el plano, y una estructura `Poligono` para representar polígonos. Luego crea un polígono de 5 vértices y calcula su área. También verifica si el punto`(0.5, 0.5)` está dentro del polígono.

Aquí hay una explicación más detallada del código:

* **Estructura `Punto`:**

    La estructura `Punto` representa un punto en el plano. Tiene dos campos: `x` e `y`, que representan las coordenadas del punto en el plano. El constructor de la estructura crea un nuevo punto con las coordenadas especificadas.

* **Estructura `Poligono`:**

    La estructura `Poligono` representa un polígono. Tiene un campo llamado `puntos`, que es un array de puntos que representan los vértices del polígono. El constructor de la estructura crea un nuevo polígono con los vértices especificados.

* **Función `calcularArea`:**

    La función `calcularArea` calcula el área del polígono. Utiliza la fórmula de Heron para calcular el área del polígono. La fórmula de Heron funciona sumando las áreas de los triángulos formados por cada lado del polígono y la diagonal que lo cruza.

* **Función `puntoDentro`:**

    La función `puntoDentro` verifica si un punto dado está dentro del polígono. Utiliza el algoritmo de intersección de líneas para verificar si hay un número impar de intersecciones entre una línea horizontal que pasa por el punto dado y los lados del polígono. Si hay un número impar de intersecciones, el punto está dentro del polígono.

* **Función `main`:**

    La función `main` es la función principal del programa. Crea un polígono de 5 vértices, calcula su área y verifica si el punto `(0.5, 0.5)` está dentro del polígono. Luego imprime los resultados en la consola.