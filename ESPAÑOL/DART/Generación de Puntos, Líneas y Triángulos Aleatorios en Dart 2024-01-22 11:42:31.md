```dart
// Importamos las librerías necesarias
import 'dart:io';
import 'dart:math';
import 'dart:async';

// Creamos una clase para representar un punto en el espacio 2D
class Punto {
  // Atributos
  final double x;
  final double y;

  // Constructor
  Punto(this.x, this.y);

  // Métodos
  @override
  String toString() {
    return '($x, $y)';
  }
}

// Creamos una clase para representar una línea en el espacio 2D
class Linea {
  // Atributos
  final Punto p1;
  final Punto p2;

  // Constructor
  Linea(this.p1, this.p2);

  // Métodos
  @override
  String toString() {
    return '[${p1.toString()} -> ${p2.toString()}]';
  }
}

// Creamos una clase para representar un triángulo en el espacio 2D
class Triangulo {
  // Atributos
  final Punto p1;
  final Punto p2;
  final Punto p3;

  // Constructor
  Triangulo(this.p1, this.p2, this.p3);

  // Métodos
  @override
  String toString() {
    return '[${p1.toString()} - ${p2.toString()} - ${p3.toString()}]';
  }

  // Calcula el área del triángulo
  double area() {
    // Usamos la fórmula de Heron
    double s = (p1.x + p2.x + p3.x) / 2;
    double a = p1.x - s;
    double b = p2.x - s;
    double c = p3.x - s;
    return sqrt(s * (s - a) * (s - b) * (s - c));
  }
}

// Creamos una función para generar un número aleatorio entre dos valores
double randomDouble(double min, double max) {
  return min + Random().nextDouble() * (max - min);
}

// Creamos una función para generar un punto aleatorio en un cuadrado
Punto randomPoint(double minX, double minY, double maxX, double maxY) {
  return Punto(randomDouble(minX, maxX), randomDouble(minY, maxY));
}

// Creamos una función para generar una línea aleatoria en un cuadrado
Linea randomLine(double minX, double minY, double maxX, double maxY) {
  return Linea(randomPoint(minX, minY, maxX, maxY), randomPoint(minX, minY, maxX, maxY));
}

// Creamos una función para generar un triángulo aleatorio en un cuadrado
Triangulo randomTriangle(double minX, double minY, double maxX, double maxY) {
  return Triangulo(
      randomPoint(minX, minY, maxX, maxY),
      randomPoint(minX, minY, maxX, maxY),
      randomPoint(minX, minY, maxX, maxY));
}

// Creamos una función principal
void main() {
  // Generamos 10 puntos aleatorios en un cuadrado de 10x10
  var puntos = List.generate(10, (_) => randomPoint(0, 0, 10, 10));

  // Generamos 10 líneas aleatorias en un cuadrado de 10x10
  var lineas = List.generate(10, (_) => randomLine(0, 0, 10, 10));

  // Generamos 10 triángulos aleatorios en un cuadrado de 10x10
  var triangulos = List.generate(10, (_) => randomTriangle(0, 0, 10, 10));

  // Imprimimos los puntos, las líneas y los triángulos generados
  print('Puntos:');
  for (var punto in puntos) {
    print(punto);
  }

  print('Líneas:');
  for (var linea in lineas) {
    print(linea);
  }

  print('Triángulos:');
  for (var triangulo in triangulos) {
    print(triangulo);
  }

  // Calculamos el área del primer triángulo
  print('Área del primer triángulo: ${triangulos[0].area()}');
}
```

**Explicación:**

Este código genera 10 puntos, 10 líneas y 10 triángulos aleatorios en un cuadrado de 10x10. Luego, imprime los puntos, las líneas y los triángulos generados. Finalmente, calcula el área del primer triángulo.

El código está organizado en clases para representar los puntos, las líneas y los triángulos. También se definen funciones para generar números aleatorios, puntos aleatorios, líneas aleatorias y triángulos aleatorios.

La función principal `main()` utiliza las funciones definidas para generar los puntos, las líneas y los triángulos aleatorios. Luego, imprime los puntos, las líneas y los triángulos generados. Finalmente, calcula el área del primer triángulo.