```dart
// Definimos una clase abstracta para representar una figura geométrica.
abstract class FiguraGeometrica {
  double obtenerArea();
  double obtenerPerimetro();
}

// Creamos una clase que represente un círculo.
class Circulo implements FiguraGeometrica {
  double radio;

  Circulo(this.radio);

  @override
  double obtenerArea() {
    return Math.pi * radio * radio;
  }

  @override
  double obtenerPerimetro() {
    return 2 * Math.pi * radio;
  }
}

// Creamos una clase que represente un rectángulo.
class Rectangulo implements FiguraGeometrica {
  double ancho;
  double alto;

  Rectangulo(this.ancho, this.alto);

  @override
  double obtenerArea() {
    return ancho * alto;
  }

  @override
  double obtenerPerimetro() {
    return 2 * (ancho + alto);
  }
}

// Creamos una clase que represente un triángulo.
class Triangulo implements FiguraGeometrica {
  double base;
  double altura;

  Triangulo(this.base, this.altura);

  @override
  double obtenerArea() {
    return 0.5 * base * altura;
  }

  @override
  double obtenerPerimetro() {
    double lado1 = Math.sqrt(base * base + altura * altura);
    double lado2 = base;
    double lado3 = altura;
    return lado1 + lado2 + lado3;
  }
}

// Creamos una función que reciba una lista de figuras geométricas y devuelva la figura con mayor área.
FiguraGeometrica figuraConMayorArea(List<FiguraGeometrica> figuras) {
  FiguraGeometrica figuraMayorArea = figuras[0];
  for (FiguraGeometrica figura in figuras) {
    if (figura.obtenerArea() > figuraMayorArea.obtenerArea()) {
      figuraMayorArea = figura;
    }
  }
  return figuraMayorArea;
}

// Creamos una función que reciba una lista de figuras geométricas y devuelva la figura con menor perímetro.
FiguraGeometrica figuraConMenorPerimetro(List<FiguraGeometrica> figuras) {
  FiguraGeometrica figuraMenorPerimetro = figuras[0];
  for (FiguraGeometrica figura in figuras) {
    if (figura.obtenerPerimetro() < figuraMenorPerimetro.obtenerPerimetro()) {
      figuraMenorPerimetro = figura;
    }
  }
  return figuraMenorPerimetro;
}

// Creamos una función principal para probar las clases y funciones definidas anteriormente.
void main() {
  // Creamos una lista de figuras geométricas.
  List<FiguraGeometrica> figuras = [
    Circulo(5.0),
    Rectangulo(3.0, 4.0),
    Triangulo(6.0, 8.0)
  ];

  // Imprimimos la figura con mayor área y la figura con menor perímetro.
  print("Figura con mayor área: ${figuraConMayorArea(figuras)}");
  print("Figura con menor perímetro: ${figuraConMenorPerimetro(figuras)}");
}
```

Explicación del código:

1. Definimos una clase abstracta llamada `FiguraGeometrica` que representa una figura geométrica. Esta clase tiene dos métodos abstractos: `obtenerArea()` y `obtenerPerimetro()`, que deben ser implementados por las clases que hereden de ella.
2. Creamos tres clases concretas que representan un círculo, un rectángulo y un triángulo. Estas clases implementan los métodos `obtenerArea()` y `obtenerPerimetro()` para calcular el área y el perímetro de cada figura, respectivamente.
3. Creamos una función llamada `figuraConMayorArea()` que recibe una lista de figuras geométricas y devuelve la figura con mayor área.
4. Creamos una función llamada `figuraConMenorPerimetro()` que recibe una lista de figuras geométricas y devuelve la figura con menor perímetro.
5. En la función principal, creamos una lista de figuras geométricas y llamamos a las funciones `figuraConMayorArea()` y `figuraConMenorPerimetro()` para imprimir la figura con mayor área y la figura con menor perímetro, respectivamente.