```dart
// Librerías
import 'dart:math';
import 'dart:io';
import 'dart:collection';

// Clase Punto
class Punto {
  double x, y;
  Punto(this.x, this.y);
}

// Clase Rectángulo
class Rectangulo {
  Punto punto1, punto2;
  Rectangulo(this.punto1, this.punto2);

  double area() => (punto2.x - punto1.x) * (punto2.y - punto1.y);
  bool contiene(Punto punto) =>
      punto.x >= punto1.x && punto.x <= punto2.x && punto.y >= punto1.y && punto.y <= punto2.y;
}

// Clase Círculo
class Circulo {
  Punto centro;
  double radio;
  Circulo(this.centro, this.radio);

  double area() => pi * radio * radio;
  bool contiene(Punto punto) => distancia(punto, centro) <= radio;
  double distancia(Punto punto1, Punto punto2) =>
      sqrt((punto2.x - punto1.x) * (punto2.x - punto1.x) + (punto2.y - punto1.y) * (punto2.y - punto1.y));
}

// Función principal
void main() {
  // Instanciación de objetos
  Rectangulo rectangulo = Rectangulo(Punto(0, 0), Punto(10, 10));
  Circulo circulo = Circulo(Punto(5, 5), 5);

  // Pedir coordenadas al usuario
  print("Introduce las coordenadas del punto (separadas por espacios):");
  String input = stdin.readLineSync()!;
  List<String> coordenadas = input.split(' ');
  Punto punto = Punto(double.parse(coordenadas[0]), double.parse(coordenadas[1]));

  // Comprobar si el punto está dentro de las figuras
  if (rectangulo.contiene(punto)) {
    print("El punto está dentro del rectángulo.");
  } else if (circulo.contiene(punto)) {
    print("El punto está dentro del círculo.");
  } else {
    print("El punto está fuera de las figuras.");
  }

  // Mostrar el área de las figuras
  print("Área del rectángulo: ${rectangulo.area()}");
  print("Área del círculo: ${circulo.area()}");
}
```

Explicación del código:

* La clase `Punto` representa un punto en el plano cartesiano.
* La clase `Rectangulo` representa un rectángulo en el plano cartesiano.
* La clase `Circulo` representa un círculo en el plano cartesiano.
* La función `main()` es el punto de entrada del programa.
* Se crean instancias de las clases `Rectangulo` y `Circulo`.
* Se pide al usuario que introduzca las coordenadas de un punto.
* Se comprueba si el punto está dentro del rectángulo o del círculo.
* Se muestra el área del rectángulo y del círculo.