```dart
// Importamos las librerías necesarias
import 'dart:collection';
import 'dart:math';

// Definimos una clase para representar un punto en el espacio
class Punto {
  double x;
  double y;

  // Constructor de la clase Punto
  Punto(this.x, this.y);

  // Método para calcular la distancia entre dos puntos
  double distancia(Punto otro) {
    return sqrt(pow(x - otro.x, 2) + pow(y - otro.y, 2));
  }
}

// Definimos una clase para representar una línea en el espacio
class Linea {
  Punto punto1;
  Punto punto2;

  // Constructor de la clase Línea
  Linea(this.punto1, this.punto2);

  // Método para calcular la longitud de la línea
  double longitud() {
    return punto1.distancia(punto2);
  }
}

// Definimos una clase para representar un triángulo en el espacio
class Triangulo {
  Punto vertice1;
  Punto vertice2;
  Punto vertice3;

  // Constructor de la clase Triángulo
  Triangulo(this.vertice1, this.vertice2, this.vertice3);

  // Método para calcular el área del triángulo
  double area() {
    double semiperimetro = (vertice1.distancia(vertice2) + vertice2.distancia(vertice3) + vertice3.distancia(vertice1)) / 2;
    return sqrt(semiperimetro * (semiperimetro - vertice1.distancia(vertice2)) * (semiperimetro - vertice2.distancia(vertice3)) * (semiperimetro - vertice3.distancia(vertice1)));
  }

  // Método para determinar si el triángulo es equilátero
  bool esEquilatero() {
    return vertice1.distancia(vertice2) == vertice2.distancia(vertice3) && vertice2.distancia(vertice3) == vertice3.distancia(vertice1);
  }

}

// Definimos una clase para representar un círculo en el espacio
class Circulo {
  Punto centro;
  double radio;

  // Constructor de la clase Círculo
  Circulo(this.centro, this.radio);

  // Método para calcular el área del círculo
  double area() {
    return PI * pow(radio, 2);
  }
}

// Definimos una clase para representar un cuadrado en el espacio
class Cuadrado {
  Punto vertice1;
  Punto vertice2;
  Punto vertice3;
  Punto vertice4;

  // Constructor de la clase Cuadrado
  Cuadrado(this.vertice1, this.vertice2, this.vertice3, this.vertice4);

  // Método para calcular el área del cuadrado
  double area() {
    return pow(vertice1.distancia(vertice2), 2);
  }
}

// Definimos una clase para representar un cubo en el espacio
class Cubo {
  Punto vertice1;
  Punto vertice2;
  Punto vertice3;
  Punto vertice4;
  Punto vertice5;
  Punto vertice6;

  // Constructor de la clase Cubo
  Cubo(this.vertice1, this.vertice2, this.vertice3, this.vertice4, this.vertice5, this.vertice6);

  // Método para calcular el volumen del cubo
  double volumen() {
    return pow(vertice1.distancia(vertice2), 3);
  }
}

// Definimos una clase para representar una esfera en el espacio
class Esfera {
  Punto centro;
  double radio;

  // Constructor de la clase Esfera
  Esfera(this.centro, this.radio);

  // Método para calcular el volumen de la esfera
  double volumen() {
    return (4 / 3) * PI * pow(radio, 3);
  }
}

// Creamos un objeto de la clase Punto
Punto punto1 = Punto(0, 0);

// Creamos un objeto de la clase Línea
Linea linea1 = Linea(punto1, Punto(10, 10));

// Creamos un objeto de la clase Triángulo