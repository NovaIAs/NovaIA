```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <cassert>
#include <string>

using namespace std;

// Clase abstracta para representar una figura geométrica.
class Figura {
public:
  // Método virtual puro para calcular el área de la figura.
  virtual double area() const = 0;

  // Método virtual puro para calcular el perímetro de la figura.
  virtual double perimetro() const = 0;
};

// Clase que representa un círculo.
class Circulo : public Figura {
public:
  // Constructor que recibe el radio del círculo.
  Circulo(double radio) : _radio(radio) {}

  // Método para calcular el área del círculo.
  double area() const override {
    return M_PI * _radio * _radio;
  }

  // Método para calcular el perímetro del círculo.
  double perimetro() const override {
    return 2 * M_PI * _radio;
  }

private:
  // Radio del círculo.
  double _radio;
};

// Clase que representa un rectángulo.
class Rectangulo : public Figura {
public:
  // Constructor que recibe la base y la altura del rectángulo.
  Rectangulo(double base, double altura) : _base(base), _altura(altura) {}

  // Método para calcular el área del rectángulo.
  double area() const override {
    return _base * _altura;
  }

  // Método para calcular el perímetro del rectángulo.
  double perimetro() const override {
    return 2 * (_base + _altura);
  }

private:
  // Base del rectángulo.
  double _base;

  // Altura del rectángulo.
  double _altura;
};

// Clase que representa un triángulo.
class Triangulo : public Figura {
public:
  // Constructor que recibe los lados del triángulo.
  Triangulo(double lado1, double lado2, double lado3)
    : _lado1(lado1), _lado2(lado2), _lado3(lado3) {
    // Comprobamos que los lados son válidos para formar un triángulo.
    assert(_lado1 + _lado2 > _lado3 && _lado1 + _lado3 > _lado2 && _lado2 + _lado3 > _lado1);
  }

  // Método para calcular el área del triángulo.
  double area() const override {
    // Calculamos el semiperímetro del triángulo.
    double s = (_lado1 + _lado2 + _lado3) / 2;

    // Calculamos el área del triángulo utilizando la fórmula de Herón.
    return sqrt(s * (s - _lado1) * (s - _lado2) * (s - _lado3));
  }

  // Método para calcular el perímetro del triángulo.
  double perimetro() const override {
    return _lado1 + _lado2 + _lado3;
  }

private:
  // Lado 1 del triángulo.
  double _lado1;

  // Lado 2 del triángulo.
  double _lado2;

  // Lado 3 del triángulo.
  double _lado3;
};

// Función para comparar figuras por área.
bool compararPorArea(const Figura* figura1, const Figura* figura2) {
  return figura1->area() < figura2->area();
}

// Función para comparar figuras por perímetro.
bool compararPorPerimetro(const Figura* figura1, const Figura* figura2) {
  return figura1->perimetro() < figura2->perimetro();
}

int main() {
  // Creamos un vector de figuras.
  vector<Figura*> figuras;

  // Añadimos un círculo al vector.
  figuras.push_back(new Circulo(5));

  // Añadimos un rectángulo al vector.
  figuras.push_back(new Rectangulo(10, 5));

  // Añadimos un triángulo al vector.
  figuras.push_back(new Triangulo(3, 4, 5));

  // Imprimimos el área y el perímetro de cada figura.
  cout << "Áreas de las figuras:" << endl;
  for (Figura* figura : figuras) {
    cout << "  " << figura->area() << endl;
  }

  cout << "Perímetros de las figuras:" << endl;
  for (Figura* figura : figuras) {
    cout << "  " << figura->perimetro() << endl;
  }

  // Ordenamos las figuras por área.
  sort(figuras.begin(), figuras.end(), compararPorArea);

  // Imprimimos las figuras ordenadas por área.
  cout << "Figuras ordenadas por área:" << endl;
  for (Figura* figura : figuras) {
    cout << "  " << figura->area() << endl;
  }

  // Ordenamos las figuras por perímetro.
  sort(figuras.begin(), figuras.end(), compararPorPerimetro);

  // Imprimimos las figuras ordenadas por perímetro.
  cout << "Figuras ordenadas por perímetro:" << endl;
  for (Figura* figura : figuras) {
    cout << "  " << figura->perimetro() << endl;
  }

  // Liberamos la memoria de las figuras.
  for (Figura* figura : figuras) {
    delete figura;
  }

  return 0;
}
```

Explicación del código:

1. **Definición de la clase abstracta `Figura`:** Esta clase define una interfaz común para todas las clases que representen figuras geométricas. La clase abstracta `Figura` tiene dos métodos virtuales puros, `area()` y `perimetro()`, que deben ser implementados por las clases derivadas.


2. **Definición de las clases `Circulo`, `Rectangulo` y `Triangulo`:** Estas clases derivan de la clase abstracta `Figura` e implementan los métodos `area()` y `perimetro()`. Cada clase tiene sus propios atributos y métodos para calcular el área y el perímetro específicos de esa figura.


3. **Definición de las funciones `compararPorArea()` y `compararPorPerimetro()`:** Estas funciones son usadas para comparar figuras por área y perímetro, respectivamente. Se utilizan en la función `sort()` para ordenar las figuras según el criterio especificado.


4. **Creación de un vector de figuras:** Se crea un vector de punteros a objetos de la clase `Figura`. A continuación, se añaden al vector objetos de las clases `Circulo`, `Rectangulo` y `Triangulo`.


5. **Impresión del área y el perímetro de cada figura:** Se recorre el vector de figuras e imprime el área y el perímetro de cada figura.


6. **Ordenación de las figuras por área y perímetro:** Se utilizan las funciones `sort()` y `compararPorArea()` y `compararPorPerimetro()` para ordenar las figuras por área y perímetro, respectivamente.


7. **Impresión de las figuras ordenadas por área y perímetro:** Se recorre el vector de figuras e imprime las figuras ordenadas por área y perímetro, respectivamente.


8. **Liberación de la memoria de las figuras:** Una vez que se ha terminado de usar el vector de figuras, se recorre el vector y se libera la memoria de cada figura.