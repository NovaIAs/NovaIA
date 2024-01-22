```dart
// Importamos las bibliotecas necesarias.
import 'dart:io';
import 'dart:math';

// Creamos una clase para representar un punto en el espacio tridimensional.
class Punto3D {
  double x;
  double y;
  double z;

  // Constructor de la clase.
  Punto3D(this.x, this.y, this.z);

  // Método para calcular la distancia entre dos puntos.
  double distancia(Punto3D otroPunto) {
    return sqrt(pow(x - otroPunto.x, 2) + pow(y - otroPunto.y, 2) + pow(z - otroPunto.z, 2));
  }

  // Método para imprimir el punto en la consola.
  void imprimir() {
    print('($x, $y, $z)');
  }
}

// Creamos una clase para representar una línea en el espacio tridimensional.
class Linea3D {
  Punto3D puntoInicial;
  Punto3D puntoFinal;

  // Constructor de la clase.
  Linea3D(this.puntoInicial, this.puntoFinal);

  // Método para calcular la longitud de la línea.
  double longitud() {
    return puntoInicial.distancia(puntoFinal);
  }

  // Método para imprimir la línea en la consola.
  void imprimir() {
    print('Línea desde ${puntoInicial.x}, ${puntoInicial.y}, ${puntoInicial.z} hasta ${puntoFinal.x}, ${puntoFinal.y}, ${puntoFinal.z}');
  }
}

// Creamos una clase para representar un plano en el espacio tridimensional.
class Plano3D {
  Punto3D punto;
  Vector3D normal;

  // Constructor de la clase.
  Plano3D(this.punto, this.normal);

  // Método para calcular la distancia entre un punto y el plano.
  double distancia(Punto3D punto) {
    return normal.x * (punto.x - this.punto.x) + normal.y * (punto.y - this.punto.y) + normal.z * (punto.z - this.punto.z);
  }

  // Método para imprimir el plano en la consola.
  void imprimir() {
    print('Plano con punto ${punto.x}, ${punto.y}, ${punto.z} y normal ${normal.x}, ${normal.y}, ${normal.z}');
  }
}

// Creamos una clase para representar un vector en el espacio tridimensional.
class Vector3D {
  double x;
  double y;
  double z;

  // Constructor de la clase.
  Vector3D(this.x, this.y, this.z);

  // Método para calcular la magnitud del vector.
  double magnitud() {
    return sqrt(pow(x, 2) + pow(y, 2) + pow(z, 2));
  }

  // Método para imprimir el vector en la consola.
  void imprimir() {
    print('($x, $y, $z)');
  }
}

// Creamos una función para generar un número aleatorio entre dos valores.
double generarAleatorio(double min, double max) {
  return min + Random().nextDouble() * (max - min);
}

// Creamos una función para crear un punto aleatorio en el espacio tridimensional.
Punto3D generarPuntoAleatorio() {
  return Punto3D(generarAleatorio(-10, 10), generarAleatorio(-10, 10), generarAleatorio(-10, 10));
}

// Creamos una función para crear una línea aleatoria en el espacio tridimensional.
Linea3D generarLineaAleatoria() {
  return Linea3D(generarPuntoAleatorio(), generarPuntoAleatorio());
}

// Creamos una función para crear un plano aleatorio en el espacio tridimensional.
Plano3D generarPlanoAleatorio() {
  return Plano3D(generarPuntoAleatorio(), Vector3D(generarAleatorio(-1, 1), generarAleatorio(-1, 1), generarAleatorio(-1, 1)));
}

// Creamos una función para imprimir un conjunto de puntos en la consola.
void imprimirPuntos(List<Punto3D> puntos) {
  for (var punto in puntos) {
    punto.imprimir();
  }
}

// Creamos una función para imprimir un conjunto de líneas en la consola.
void imprimirLineas(List<Linea3D> lineas) {
  for (var linea in lineas) {
    linea.imprimir();
  }
}

// Creamos una función para imprimir un conjunto de planos en la consola.
void imprimirPlanos(List<Plano3D> planos) {
  for (var plano in planos) {
    plano.imprimir();
  }
}

// Creamos una función para calcular la distancia entre dos conjuntos de puntos.
double distanciaEntrePuntos(List<Punto3D> puntos1, List<Punto3D> puntos2) {
  double distanciaMinima = double.infinity;
  for (var punto1 in puntos1) {
    for (var punto2 in puntos2) {
      double distancia = punto1.distancia(punto2);
      if (distancia < distanciaMinima) {
        distanciaMinima = distancia;
      }
    }
  }
  return distanciaMinima;
}

// Creamos una función para calcular la distancia entre un conjunto de puntos y un plano.
double distanciaEntrePuntosYPlano(List<Punto3D> puntos, Plano3D plano) {
  double distanciaMinima = double.infinity;
  for (var punto in puntos) {
