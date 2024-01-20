```c++
#include <iostream>
#include <vector>
#include <map>
#include <algorithm>
#include <cmath>

using namespace std;

// Definición de la clase Punto
class Punto {
public:
  double x, y;

  Punto() : x(0), y(0) {}
  Punto(double x, double y) : x(x), y(y) {}

  // Sobrecarga del operador + para sumar dos puntos
  Punto operator+(const Punto& otro) const {
    return Punto(x + otro.x, y + otro.y);
  }

  // Sobrecarga del operador - para restar dos puntos
  Punto operator-(const Punto& otro) const {
    return Punto(x - otro.x, y - otro.y);
  }

  // Sobrecarga del operador * para multiplicar un punto por un escalar
  Punto operator*(double escalar) const {
    return Punto(x * escalar, y * escalar);
  }

  // Sobrecarga del operador / para dividir un punto por un escalar
  Punto operator/(double escalar) const {
    return Punto(x / escalar, y / escalar);
  }

  // Sobrecarga del operador == para comparar dos puntos
  bool operator==(const Punto& otro) const {
    return x == otro.x && y == otro.y;
  }

  // Sobrecarga del operador != para comparar dos puntos
  bool operator!=(const Punto& otro) const {
    return !(*this == otro);
  }

  // Sobrecarga del operador << para imprimir un punto
  friend ostream& operator<<(ostream& os, const Punto& punto) {
    os << "(" << punto.x << ", " << punto.y << ")";
    return os;
  }
};

// Definición de la clase Línea
class Linea {
public:
  Punto p1, p2;

  Linea() : p1(0, 0), p2(0, 0) {}
  Linea(const Punto& p1, const Punto& p2) : p1(p1), p2(p2) {}

  // Sobrecarga del operador << para imprimir una línea
  friend ostream& operator<<(ostream& os, const Linea& linea) {
    os << "Línea: " << linea.p1 << " -> " << linea.p2;
    return os;
  }

  // Función para calcular la longitud de la línea
  double longitud() const {
    return sqrt(pow(p2.x - p1.x, 2) + pow(p2.y - p1.y, 2));
  }

  // Función para calcular la pendiente de la línea
  double pendiente() const {
    if (p2.x - p1.x == 0) {
      return INFINITY;
    }
    return (p2.y - p1.y) / (p2.x - p1.x);
  }

  // Función para determinar si la línea es paralela al eje X
  bool esParalelaAlEjeX() const {
    return pendiente() == 0;
  }

  // Función para determinar si la línea es paralela al eje Y
  bool esParalelaAlEjeY() const {
    return pendiente() == INFINITY;
  }
};

// Función para encontrar el punto de intersección de dos líneas
Punto interseccionDeLineas(const Linea& l1, const Linea& l2) {
  // Comprobar si las líneas son paralelas
  if (l1.esParalelaAlEjeX() && l2.esParalelaAlEjeX()) {
    throw invalid_argument("Las líneas son paralelas al eje X y no tienen punto de intersección.");
  }
  if (l1.esParalelaAlEjeY() && l2.esParalelaAlEjeY()) {
    throw invalid_argument("Las líneas son paralelas al eje Y y no tienen punto de intersección.");
  }

  // Calcular el punto de intersección
  double x = ((l1.p2.y - l1.p1.y) * (l2.p2.x - l2.p1.x) - (l1.p2.x - l1.p1.x) * (l2.p2.y - l2.p1.y)) /
              ((l1.p2.x - l1.p1.x) * (l2.p1.y - l2.p2.y) - (l1.p2.y - l1.p1.y) * (l2.p1.x - l2.p2.x));
  double y = ((l1.p2.x - l1.p1.x) * (l2.p2.y - l2.p1.y) - (l1.p2.y - l1.p1.y) * (l2.p2.x - l2.p1.x)) /
              ((l1.p2.x - l1.p1.x) * (l2.p1.y - l2.p2.y) - (l1.p2.y - l1.p1.y) * (l2.p1.x - l2.p2.x));

  return Punto(x, y);
}

// Función principal
int main() {
  // Crear dos puntos
  Punto p1(1, 2);
  Punto p2(3, 4);

  // Crear una línea con los dos puntos
  Linea l1(p1, p2);

  // Imprimir la línea
  cout << "Línea 1: " << l1 << endl;

  // Calcular la longitud de la línea
  cout << "Longitud de la línea 1: " << l1.longitud() << endl;

  // Calcular la pendiente de la línea
  cout << "Pendiente de la línea 1: " << l1.pendiente() << endl;

  // Determinar si la línea es paralela al eje X
  cout << "¿La línea 1 es paralela al eje X?: " << (l1.esParalelaAlEjeX() ? "Sí" : "No") << endl;

  // Determinar si la línea es paralela al eje Y
  cout << "¿La línea 1 es paralela al eje Y?: " << (l1.esParalelaAlEjeY() ? "Sí" : "No") << endl;

  // Crear un segundo punto
  Punto p3(5, 6);

  // Crear una segunda línea con los dos puntos
  Linea l2(p2, p3);

  // Imprimir la segunda línea
  cout << "Línea 2: " << l2 << endl;

  // Calcular la longitud de la segunda línea
  cout << "Longitud de la línea 2: " << l2.longitud() << endl;

  // Calcular la pendiente de la segunda línea
  cout << "Pendiente de la línea 2: " << l2.pendiente() << endl;

  // Determinar si la segunda línea es paralela al eje X
  cout << "¿La línea 2 es paralela al eje X?: " << (l2.esParalelaAlEjeX() ? "Sí" : "No") << endl;

  // Determinar si la segunda línea es paralela al eje Y
  cout << "¿La línea 2 es paralela al eje Y?: " << (l2.esParalelaAlEjeY() ? "Sí" : "No") << endl;

  // Encontrar el punto de intersección de las dos líneas
  Punto puntoDeInterseccion = interseccionDeLineas(l1, l2);

  // Imprimir el punto de intersección
  cout << "Punto de intersección de las dos líneas: " << puntoDeInterseccion << endl;

  return 0;
}
```

Este código define dos clases: `Punto` y `Línea`. La clase `Punto` representa un punto en el espacio bidimensional, con coordenadas `x` e `y`. La clase `Línea` representa una línea en el espacio bidimensional, con dos puntos: `p1` y `p2`.

El código también define una función `interseccionDeLineas()` que calcula el punto de intersección de dos líneas. Esta función toma dos líneas como argumentos y devuelve el punto de intersección de las dos líneas.

Finalmente, el código crea dos puntos, dos líneas y encuentra el punto de intersección de las dos líneas. Imprime el punto de intersección en la consola.