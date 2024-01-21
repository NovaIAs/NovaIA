```dart
import 'dart:math';

/// Clase que representa un punto en el espacio bidimensional.
class Punto {
  double x;
  double y;

  Punto(this.x, this.y);

  /// Suma las coordenadas del punto actual con las de otro punto.
  void sumar(Punto otro) {
    this.x += otro.x;
    this.y += otro.y;
  }

  /// Resta las coordenadas del punto actual con las de otro punto.
  void restar(Punto otro) {
    this.x -= otro.x;
    this.y -= otro.y;
  }

  /// Multiplica las coordenadas del punto actual por un escalar.
  void multiplicar(double escalar) {
    this.x *= escalar;
    this.y *= escalar;
  }

  /// Divide las coordenadas del punto actual por un escalar.
  void dividir(double escalar) {
    this.x /= escalar;
    this.y /= escalar;
  }

  /// Calcula la distancia entre el punto actual y otro punto.
  double distancia(Punto otro) {
    return sqrt(pow(this.x - otro.x, 2) + pow(this.y - otro.y, 2));
  }

  /// Calcula el ángulo entre el punto actual y otro punto.
  double angulo(Punto otro) {
    return atan2(otro.y - this.y, otro.x - this.x);
  }

  /// Devuelve una copia del punto actual.
  Punto copiar() {
    return Punto(this.x, this.y);
  }
}

/// Clase que representa un vector en el espacio bidimensional.
class Vector {
  Punto origen;
  Punto destino;

  Vector(this.origen, this.destino);

  /// Calcula la magnitud del vector.
  double magnitud() {
    return sqrt(pow(this.destino.x - this.origen.x, 2) + pow(this.destino.y - this.origen.y, 2));
  }

  /// Calcula el ángulo del vector con respecto al eje x.
  double angulo() {
    return atan2(this.destino.y - this.origen.y, this.destino.x - this.origen.x);
  }

  /// Suma el vector actual con otro vector.
  void sumar(Vector otro) {
    this.destino.sumar(otro.destino);
  }

  /// Resta el vector actual con otro vector.
  void restar(Vector otro) {
    this.destino.restar(otro.destino);
  }

  /// Multiplica el vector actual por un escalar.
  void multiplicar(double escalar) {
    this.destino.multiplicar(escalar);
  }

  /// Divide el vector actual por un escalar.
  void dividir(double escalar) {
    this.destino.dividir(escalar);
  }

  /// Devuelve una copia del vector actual.
  Vector copiar() {
    return Vector(this.origen, this.destino);
  }
}

/// Clase que representa un polígono en el espacio bidimensional.
class Poligono {
  List<Punto> vertices;

  Poligono(this.vertices);

  /// Calcula el perímetro del polígono.
  double perimetro() {
    double perimetro = 0;
    for (int i = 0; i < this.vertices.length; i++) {
      perimetro += this.vertices[i].distancia(this.vertices[(i + 1) % this.vertices.length]);
    }
    return perimetro;
  }

  /// Calcula el área del polígono.
  double area() {
    double area = 0;
    for (int i = 0; i < this.vertices.length; i++) {
      area += this.vertices[i].x * this.vertices[(i + 1) % this.vertices.length].y - this.vertices[i].y * this.vertices[(i + 1) % this.vertices.length].x;
    }
    return abs(area) / 2;
  }

  /// Calcula el centroide del polígono.
  Punto centroide() {
    Punto centroide = Punto(0, 0);
    for (int i = 0; i < this.vertices.length; i++) {
      centroide.sumar(this.vertices[i]);
    }
    centroide.dividir(this.vertices.length);
    return centroide;
  }

  /// Devuelve una copia del polígono actual.
  Poligono copiar() {
    List<Punto> vertices = [];
    for (int i = 0; i < this.vertices.length; i++) {
      vertices.add(this.vertices[i].copiar());
    }
    return Poligono(vertices);
  }
}

/// Crea un polígono regular con un número dado de lados y una longitud de lado dada.
Poligono crearPoligonoRegular(int numLados, double longitudLado) {
  List<Punto> vertices