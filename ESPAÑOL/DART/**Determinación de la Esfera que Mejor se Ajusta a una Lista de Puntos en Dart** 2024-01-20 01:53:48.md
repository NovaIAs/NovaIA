**Código complejo en Dart**

```dart
// Importamos las bibliotecas necesarias
import 'dart:async';
import 'dart:math';
import 'dart:io';

// Creamos una clase que represente un punto en el espacio 3D
class Punto3D {
  double x, y, z;

  Punto3D(this.x, this.y, this.z);

  // Creamos una función para calcular la distancia entre dos puntos
  double distancia(Punto3D otroPunto) {
    return sqrt(pow(x - otroPunto.x, 2) + pow(y - otroPunto.y, 2) + pow(z - otroPunto.z, 2));
  }
}

// Creamos una clase que represente una esfera
class Esfera {
  Punto3D centro;
  double radio;

  Esfera(this.centro, this.radio);

  // Creamos una función para calcular el volumen de la esfera
  double volumen() {
    return (4 / 3) * pi * pow(radio, 3);
  }

  // Creamos una función para calcular el área de la superficie de la esfera
  double superficie() {
    return 4 * pi * pow(radio, 2);
  }
}

// Creamos una función para generar un número aleatorio entre dos valores
double numeroAleatorio(double min, double max) {
  return min + (max - min) * Random().nextDouble();
}

// Creamos una función para generar una lista de puntos aleatorios en el espacio 3D
List<Punto3D> listaPuntosAleatorios(int cantidad, double min, double max) {
  List<Punto3D> lista = [];
  for (int i = 0; i < cantidad; i++) {
    lista.add(Punto3D(numeroAleatorio(min, max), numeroAleatorio(min, max), numeroAleatorio(min, max)));
  }
  return lista;
}

// Creamos una función para calcular el centroide de una lista de puntos
Punto3D centroide(List<Punto3D> lista) {
  double x = 0, y = 0, z = 0;
  for (Punto3D punto in lista) {
    x += punto.x;
    y += punto.y;
    z += punto.z;
  }
  return Punto3D(x / lista.length, y / lista.length, z / lista.length);
}

// Creamos una función para calcular la esfera que mejor se ajusta a una lista de puntos
Esfera esferaAjustada(List<Punto3D> lista) {
  Punto3D centroide = centroide(lista);
  double radioMaximo = 0;
  for (Punto3D punto in lista) {
    double distancia = centroide.distancia(punto);
    if (distancia > radioMaximo) {
      radioMaximo = distancia;
    }
  }
  return Esfera(centroide, radioMaximo);
}

// Creamos una función principal para probar el código
void main() {
  // Generamos una lista de 100 puntos aleatorios en el espacio 3D
  List<Punto3D> listaPuntos = listaPuntosAleatorios(100, -10, 10);

  // Calculamos el centroide de la lista de puntos
  Punto3D centroide = centroide(listaPuntos);

  // Calculamos la esfera que mejor se ajusta a la lista de puntos
  Esfera esferaAjustada = esferaAjustada(listaPuntos);

  // Imprimimos los resultados
  print("Centroide: (${centroide.x}, ${centroide.y}, ${centroide.z})");
  print("Radio de la esfera ajustada: ${esferaAjustada.radio}");
  print("Volumen de la esfera ajustada: ${esferaAjustada.volumen()}");
  print("Superficie de la esfera ajustada: ${esferaAjustada.superficie()}");
}
```

**Explicación del código**

El código anterior crea una clase `Punto3D` que representa un punto en el espacio 3D, y una clase `Esfera` que representa una esfera. Luego, genera una lista de 100 puntos aleatorios en el espacio 3D y calcula el centroide de la lista de puntos. Finalmente, calcula la esfera que mejor se ajusta a la lista de puntos e imprime los resultados.

El código está dividido en las siguientes partes:

* **Importación de bibliotecas:** Se importan las bibliotecas necesarias para el código, como `dart:math` para generar números aleatorios y `dart:io` para imprimir los resultados.
* **Definición de clases:** Se definen las clases `Punto3D` y `Esfera`, que representan un punto en el espacio 3D y una esfera, respectivamente.
* **Funciones auxiliares:** Se definen funciones auxiliares para generar un número aleatorio entre dos valores, generar una lista de puntos aleatorios en el espacio 3D, calcular el centroide de una lista de puntos y calcular la esfera que mejor se ajusta a una lista de puntos.
* **Función principal:** La función `main` es el punto de entrada del programa. Genera una lista de 100 puntos aleatorios en el espacio 3D, calcula el centroide de la lista de puntos y calcula la esfera que mejor se ajusta a la lista de puntos. Finalmente, imprime los resultados.