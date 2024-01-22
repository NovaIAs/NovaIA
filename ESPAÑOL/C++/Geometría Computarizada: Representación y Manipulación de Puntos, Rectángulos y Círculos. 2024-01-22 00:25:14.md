```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <string>
#include <sstream>
#include <cmath>

using namespace std;

// Definición de la clase Punto
class Punto {
public:
    double x, y;

    // Constructor por defecto
    Punto() : x(0), y(0) {}

    // Constructor que recibe las coordenadas del punto
    Punto(double x, double y) : x(x), y(y) {}

    // Método para calcular la distancia entre dos puntos
    double distancia(const Punto& otro) const {
        return sqrt(pow(x - otro.x, 2) + pow(y - otro.y, 2));
    }

    // Método para imprimir las coordenadas del punto
    void imprimir() const {
        cout << "(" << x << ", " << y << ")";
    }
};

// Definición de la clase Rectangulo
class Rectangulo {
public:
    Punto esquinaInferiorIzquierda, esquinaSuperiorDerecha;

    // Constructor por defecto
    Rectangulo() : esquinaInferiorIzquierda(0, 0), esquinaSuperiorDerecha(0, 0) {}

    // Constructor que recibe los puntos de las esquinas del rectángulo
    Rectangulo(const Punto& esquinaInferiorIzquierda, const Punto& esquinaSuperiorDerecha) : esquinaInferiorIzquierda(esquinaInferiorIzquierda), esquinaSuperiorDerecha(esquinaSuperiorDerecha) {}

    // Método para calcular el área del rectángulo
    double area() const {
        return (esquinaSuperiorDerecha.x - esquinaInferiorIzquierda.x) * (esquinaSuperiorDerecha.y - esquinaInferiorIzquierda.y);
    }

    // Método para determinar si un punto está dentro del rectángulo
    bool contiene(const Punto& punto) const {
        return (punto.x >= esquinaInferiorIzquierda.x && punto.x <= esquinaSuperiorDerecha.x && punto.y >= esquinaInferiorIzquierda.y && punto.y <= esquinaSuperiorDerecha.y);
    }

    // Método para imprimir las coordenadas de las esquinas del rectángulo
    void imprimir() const {
        cout << "Esquina inferior izquierda: ";
        esquinaInferiorIzquierda.imprimir();
        cout << endl;
        cout << "Esquina superior derecha: ";
        esquinaSuperiorDerecha.imprimir();
        cout << endl;
    }
};

// Definición de la clase Circulo
class Circulo {
public:
    Punto centro;
    double radio;

    // Constructor por defecto
    Circulo() : centro(0, 0), radio(0) {}

    // Constructor que recibe el centro y el radio del círculo
    Circulo(const Punto& centro, double radio) : centro(centro), radio(radio) {}

    // Método para calcular el área del círculo
    double area() const {
        return M_PI * pow(radio, 2);
    }

    // Método para determinar si un punto está dentro del círculo
    bool contiene(const Punto& punto) const {
        return (punto.distancia(centro) <= radio);
    }

    // Método para imprimir las coordenadas del centro y el radio del círculo
    void imprimir() const {
        cout << "Centro: ";
        centro.imprimir();
        cout << endl;
        cout << "Radio: " << radio << endl;
    }
};

// Función principal
int main() {
    // Crear un vector de puntos
    vector<Punto> puntos = {
        Punto(1, 2),
        Punto(3, 4),
        Punto(5, 6),
        Punto(7, 8),
        Punto(9, 10)
    };

    // Crear un rectángulo
    Rectangulo rectangulo(Punto(0, 0), Punto(10, 10));

    // Crear un círculo
    Circulo circulo(Punto(5, 5), 5);

    // Imprimir los puntos
    cout << "Puntos:" << endl;
    for (const Punto& punto : puntos) {
        punto.imprimir();
        cout << endl;
    }

    // Imprimir el rectángulo
    cout << "Rectángulo:" << endl;
    rectangulo.imprimir();

    // Imprimir el círculo
    cout << "Círculo:" << endl;
    circulo.imprimir();

    // Determinar qué puntos están dentro del rectángulo y el círculo
    cout << "Puntos dentro del rectángulo:" << endl;
    for (const Punto& punto : puntos) {
        if (rectangulo.contiene(punto)) {
            punto.imprimir();
            cout << endl;
        }
    }

    cout << "Puntos dentro del círculo:" << endl;
    for (const Punto& punto : puntos) {
        if (circulo.contiene(punto)) {
            punto.imprimir();
            cout << endl;
        }
    }

    return 0;
}

```

Explicación del código:

* La clase `Punto` representa un punto en el espacio bidimensional. Tiene dos atributos públicos: `x` e `y`, que representan las coordenadas del punto. Tiene un constructor por defecto y un constructor que recibe las coordenadas del punto. También tiene un método `distancia` que calcula la distancia entre dos puntos y un método `imprimir` que imprime las coordenadas del punto.

* La clase `Rectangulo` representa un rectángulo en el espacio bidimensional. Tiene dos atributos públicos: `esquinaInferiorIzquierda` y `esquinaSuperiorDerecha`, que representan las dos esquinas del rectángulo. Tiene un constructor por defecto y un constructor que recibe las dos esquinas del rectángulo. También tiene un método `area` que calcula el área del rectángulo, un método `contiene` que determina si un punto está dentro del rectángulo y un método `imprimir` que imprime las coordenadas de las dos esquinas del rectángulo.

* La clase `Circulo` representa un círculo en el espacio bidimensional. Tiene dos atributos públicos: `centro` y `radio`, que representan el centro y el radio del círculo. Tiene un constructor por defecto y un constructor que recibe el centro y el radio del círculo. También tiene un método `area` que calcula el área del círculo, un método `contiene` que determina si un punto está dentro del círculo y un método `imprimir` que imprime las coordenadas del centro y el radio del círculo.

* La función `main` crea un vector de puntos, un rectángulo y un círculo. Luego, imprime los puntos, el rectángulo y el círculo. A continuación, determina qué puntos están dentro del rectángulo y el círculo e imprime los resultados.