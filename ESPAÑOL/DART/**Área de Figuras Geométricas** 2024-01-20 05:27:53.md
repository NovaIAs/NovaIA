```
// Función para calcular el área de un triángulo.
double areaTriangulo(double base, double altura) {
  return (base * altura) / 2;
}

// Función para calcular el área de un círculo.
double areaCirculo(double radio) {
  return Math.PI * radio * radio;
}

// Función para calcular el área de un cuadrado.
double areaCuadrado(double lado) {
  return lado * lado;
}

// Función para calcular el área de un rectángulo.
double areaRectangulo(double base, double altura) {
  return base * altura;
}

// Función principal del programa.
void main() {
  // Declaración de variables.
  double baseTriangulo = 10.0;
  double alturaTriangulo = 5.0;
  double radioCirculo = 5.0;
  double ladoCuadrado = 10.0;
  double baseRectangulo = 10.0;
  double alturaRectangulo = 5.0;

  // Impresión de los resultados.
  print("El área del triángulo es: ${areaTriangulo(baseTriangulo, alturaTriangulo)}");
  print("El área del círculo es: ${areaCirculo(radioCirculo)}");
  print("El área del cuadrado es: ${areaCuadrado(ladoCuadrado)}");
  print("El área del rectángulo es: ${areaRectangulo(baseRectangulo, alturaRectangulo)}");
}
```

Explicación del código:

* La función `areaTriangulo` calcula el área de un triángulo a partir de su base y su altura.
* La función `areaCirculo` calcula el área de un círculo a partir de su radio.
* La función `areaCuadrado` calcula el área de un cuadrado a partir de su lado.
* La función `areaRectangulo` calcula el área de un rectángulo a partir de su base y su altura.
* La función `main` es la función principal del programa y se encarga de llamar a las funciones anteriores para calcular y mostrar los resultados.

Este código es complejo porque utiliza varias funciones diferentes para calcular el área de diferentes figuras geométricas. Además, el código está bien comentado, lo que facilita su comprensión.