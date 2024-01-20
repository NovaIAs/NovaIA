```dart
// Importamos las bibliotecas necesarias.
import 'dart:io';
import 'dart:math';
import 'dart:collection';

// Definimos una clase abstracta para representar una figura geométrica.
abstract class FiguraGeometrica {
  // Definimos un método abstracto para calcular el área de la figura.
  double calcularArea();

  // Definimos un método abstracto para calcular el perímetro de la figura.
  double calcularPerimetro();
}

// Definimos una clase para representar un círculo.
class Circulo extends FiguraGeometrica {
  // Definimos el radio del círculo.
  double radio;

  // Definimos el constructor del círculo.
  Circulo(this.radio);

  // Implementamos el método para calcular el área del círculo.
  @override
  double calcularArea() {
    return pi * pow(radio, 2);
  }

  // Implementamos el método para calcular el perímetro del círculo.
  @override
  double calcularPerimetro() {
    return 2 * pi * radio;
  }
}

// Definimos una clase para representar un cuadrado.
class Cuadrado extends FiguraGeometrica {
  // Definimos el lado del cuadrado.
  double lado;

  // Definimos el constructor del cuadrado.
  Cuadrado(this.lado);

  // Implementamos el método para calcular el área del cuadrado.
  @override
  double calcularArea() {
    return pow(lado, 2);
  }

  // Implementamos el método para calcular el perímetro del cuadrado.
  @override
  double calcularPerimetro() {
    return 4 * lado;
  }
}

// Definimos una clase para representar un rectángulo.
class Rectangulo extends FiguraGeometrica {
  // Definimos el largo y el ancho del rectángulo.
  double largo;
  double ancho;

  // Definimos el constructor del rectángulo.
  Rectangulo(this.largo, this.ancho);

  // Implementamos el método para calcular el área del rectángulo.
  @override
  double calcularArea() {
    return largo * ancho;
  }

  // Implementamos el método para calcular el perímetro del rectángulo.
  @override
  double calcularPerimetro() {
    return 2 * (largo + ancho);
  }
}

// Definimos una clase para representar un triángulo.
class Triangulo extends FiguraGeometrica {
  // Definimos los lados del triángulo.
  double lado1;
  double lado2;
  double lado3;

  // Definimos el constructor del triángulo.
  Triangulo(this.lado1, this.lado2, this.lado3);

  // Implementamos el método para calcular el área del triángulo.
  @override
  double calcularArea() {
    // Calculamos el semiperímetro del triángulo.
    double semiperimetro = (lado1 + lado2 + lado3) / 2;

    // Calculamos el área del triángulo usando la fórmula de Heron.
    return sqrt(semiperimetro * (semiperimetro - lado1) * (semiperimetro - lado2) * (semiperímetro - lado3));
  }

  // Implementamos el método para calcular el perímetro del triángulo.
  @override
  double calcularPerimetro() {
    return lado1 + lado2 + lado3;
  }
}

// Definimos una clase para representar una colección de figuras geométricas.
class ColeccionFiguras {
  // Definimos una lista de figuras geométricas.
  List<FiguraGeometrica> figuras;

  // Definimos el constructor de la colección de figuras.
  ColeccionFiguras(this.figuras);

  // Definimos un método para calcular el área total de la colección de figuras.
  double calcularAreaTotal() {
    // Inicializamos el área total a 0.
    double areaTotal = 0;

    // Iteramos sobre las figuras de la colección y sumamos sus áreas.
    for (FiguraGeometrica figura in figuras) {
      areaTotal += figura.calcularArea();
    }

    // Devolvemos el área total.
    return areaTotal;
  }

  // Definimos un método para calcular el perímetro total de la colección de figuras.
  double calcularPerimetroTotal() {
    // Inicializamos el perímetro total a 0.
    double perimetroTotal = 0;

    // Iteramos sobre las figuras de la colección y sumamos sus perímetros.
    for (FiguraGeometrica figura in figuras) {
      perimetroTotal += figura.calcularPerimetro();
    }

    // Devolvemos el perímetro total.
    return perimetroTotal;
  }
}

// Definimos una función para crear una colección de figuras geométricas aleatorias.
ColeccionFiguras crearColeccionFigurasAleatorias(int numFiguras) {
  // Creamos una lista vacía de figuras geométricas.
  List<FiguraGeometrica> figuras = [];

  // Iteramos sobre el número de figuras que queremos crear.
  for (int i = 0; i < numFiguras; i++) {
    // Generamos un número aleatorio entre 1 y 4 para determinar el tipo de figura.
    int tipoFigura = Random().nextInt(4) + 1;

    // Creamos una figura geométrica aleatoria según el tipo de figura.
    FiguraGeometrica figura;
    switch (tipoFigura) {
      case 1:
        figura = Circulo(Random().nextDouble() * 10);
        break;
      case 2:
        figura = Cuadrado(Random().nextDouble() * 10);
        break;
      case 3:
        figura = Rectangulo(Random().nextDouble() * 10, Random().nextDouble() * 10);
        break;
      case 4:
        figura = Triangulo(Random().nextDouble() * 10, Random().nextDouble() * 10, Random().nextDouble() * 10);
        break;
    }

    // Añadimos la figura a la lista de figuras.
    figuras.add(figura);
  }

  // Creamos una colección de figuras con la lista de figuras.
  ColeccionFiguras coleccionFiguras = ColeccionFiguras(figuras);

  // Devolvemos la colección de figuras.
  return coleccionFiguras;
}

// Definimos la función main del programa.
void main() {
  // Creamos una colección de figuras geométricas aleatorias con 10 figuras.
  ColeccionFiguras coleccionFiguras = crearColeccionFigurasAleatorias(10);

  // Imprimimos el área total de la colección de figuras.
  print("Área total: ${coleccionFiguras.calcularAreaTotal()}");

  // Imprimimos el perímetro total de la colección de figuras.
  print("Perímetro total: ${coleccionFiguras.calcularPerimetroTotal()}");
}
```

Explicación del código:

* Definimos una clase abstracta `FiguraGeometrica` que representa una figura geométrica. Esta clase define dos métodos abstractos, `calcularArea()` y `calcularPerimetro()`, que las clases concretas deben implementar.
* Definimos cuatro clases concretas que heredan de la clase `FiguraGeometrica`: `Circulo`, `Cuadrado`, `Rectangulo` y `Triangulo`. Cada clase concreta implementa los métodos `calcularArea()` y `calcularPerimetro()` para calcular el área y el perímetro de la figura geométrica correspondiente.
* Definimos una clase `ColeccionFiguras` que representa una colección de figuras geométricas. Esta clase tiene una lista de figuras geométricas y define dos métodos, `calcularAreaTotal()` y `calcularPerimetroTotal()`, que calculan el área total y el perímetro total de la colección de figuras, respectivamente.
* Definimos una función `crearColeccionFigurasAleatorias()` que crea una colección de figuras geométricas aleatorias. Esta función genera un número aleatorio de figuras geométricas de diferentes tipos y las añade a una lista. A continuación, crea una colección de figuras con la lista de figuras.
* En la función `main()`, creamos una colección de figuras geométricas aleatorias con 10 figuras e imprimimos el área total y el perímetro total de la colección de figuras.