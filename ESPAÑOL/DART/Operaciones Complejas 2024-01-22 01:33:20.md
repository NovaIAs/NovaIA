```dart
// Importamos las bibliotecas necesarias
import 'dart:math';
import 'dart:collection';

// Definimos una clase llamada "Complejo" que representará a los números complejos
class Complejo {
  // Atributos de la clase
  double real;
  double imaginaria;

  // Constructor de la clase
  Complejo(this.real, this.imaginaria);

  // Métodos de la clase

  // Sumamos dos números complejos
  Complejo operator +(Complejo otro) {
    return Complejo(real + otro.real, imaginaria + otro.imaginaria);
  }

  // Restamos dos números complejos
  Complejo operator -(Complejo otro) {
    return Complejo(real - otro.real, imaginaria - otro.imaginaria);
  }

  // Multiplicamos dos números complejos
  Complejo operator *(Complejo otro) {
    return Complejo(
        real * otro.real - imaginaria * otro.imaginaria,
        real * otro.imaginaria + imaginaria * otro.real);
  }

  // Dividimos dos números complejos
  Complejo operator /(Complejo otro) {
    double denominador = otro.real * otro.real + otro.imaginaria * otro.imaginaria;
    return Complejo(
        (real * otro.real + imaginaria * otro.imaginaria) / denominador,
        (imaginaria * otro.real - real * otro.imaginaria) / denominador);
  }

  // Cambiamos el signo del número complejo
  Complejo operator -() {
    return Complejo(-real, -imaginaria);
  }

  // Devolvemos la magnitud del número complejo
  double magnitud() {
    return sqrt(real * real + imaginaria * imaginaria);
  }

  // Devolvemos el argumento del número complejo
  double argumento() {
    return atan2(imaginaria, real);
  }

  // Devolvemos una cadena que representa el número complejo
  @override
  String toString() {
    return 'Complejo{real: $real, imaginaria: $imaginaria}';
  }

  @override
  bool operator ==(Object other) {
    if (identical(this, other)) {
      return true;
    }
    if (other.runtimeType != runtimeType) {
      return false;
    }
    final Complejo otroComplejo = other as Complejo;
    return real == otroComplejo.real && imaginaria == otroComplejo.imaginaria;
  }

  @override
  int get hashCode => real.hashCode ^ imaginaria.hashCode;
}

// Definimos una función que devuelve el conjugado de un número complejo
Complejo conjugado(Complejo z) {
  return Complejo(z.real, -z.imaginaria);
}

// Definimos una función que devuelve la raíz cuadrada de un número complejo
Complejo raizCuadrada(Complejo z) {
  double magnitud = z.magnitud();
  double argumento = z.argumento();
  return Complejo(sqrt(magnitud / 2) * cos(argumento / 2),
      sqrt(magnitud / 2) * sin(argumento / 2));
}

// Definimos una función que devuelve una lista con las raíces n-ésimas de un número complejo
List<Complejo> raicesN(Complejo z, int n) {
  double magnitud = z.magnitud();
  double argumento = z.argumento();
  List<Complejo> raices = [];
  for (int k = 0; k < n; k++) {
    raices.add(Complejo(
        sqrt(magnitud / n) * cos((argumento + 2 * k * pi) / n),
        sqrt(magnitud / n) * sin((argumento + 2 * k * pi) / n)));
  }
  return raices;
}

// Definimos una función que devuelve el producto escalar de dos números complejos
double productoEscalar(Complejo z1, Complejo z2) {
  return z1.real * z2.real + z1.imaginaria * z2.imaginaria;
}

// Definimos una función que devuelve el producto vectorial de dos números complejos
Complejo productoVectorial(Complejo z1, Complejo z2) {
  return Complejo(z1.real * z2.imaginaria - z1.imaginaria * z2.real,
      z1.real * z2.real + z1.imaginaria * z2.imaginaria);
}

// Definimos una clase llamada "Polinomio" que representará a los polinomios complejos
class Polinomio {
  // Atributos de la clase
  List<Complejo> coeficientes;

  // Constructor de la clase
  Polinomio(this.coeficientes);

  // Métodos de la clase

  // Evaluamos el polinomio en un número complejo
  Complejo evaluar(Complejo z) {
    Complejo resultado = Complejo(0, 0);
    for (Complejo coeficiente in coeficientes) {
      resultado = resultado * z + coeficiente;
    }
    return resultado;
  }

  // Devolvemos el grado del polinomio
  int grado() {
    return coeficientes.length - 1;
  }

  // Devolvemos una cadena que representa el polinomio
  @override
  String toString() {
    String resultado = '';
    for (int i = grado(); i >= 0; i--) {
      if (coeficientes[i] != Complejo(0, 0)) {
        resultado += '${coeficientes[i]}z^$i';
        if (i > 0) {
          resultado += ' + ';
        }
      }
    }
    return resultado;
  }
}

// Definimos una función que devuelve las raíces de un polinomio complejo
List<Complejo> raices(Polinomio p) {
  List<Complejo> raices = [];
  if (p.grado() == 1) {
    raices.add(-p.coeficientes[0] / p.coeficientes[1]);
  } else if (p.grado() == 2) {
    double a = p.coeficientes[2];
    double b = p.coeficientes[1];
    double c = p.coeficientes[0];
    double discriminante = b * b - 4 * a * c;
    if (discriminante > 0) {
      raices.add((-b + sqrt(discriminante)) / (2 * a));
      raices.add((-b - sqrt(discriminante)) / (2 * a));
    } else if (discriminante == 0) {
      raices.add(-b / (2 * a));
    }
  } else {
    throw Exception('El polinomio no tiene raíces reales');
  }
  return raices;
}

// Definimos una función que devuelve el mcd de dos números complejos
Complejo mcd(Complejo z1, Complejo z2) {
  if (z2 == Complejo(0, 0)) {
    return z1;
  } else {
    return mcd(z2, z1 % z2);
  }
}

// Definimos una función que devuelve el mcm de dos números complejos
Complejo mcm(Complejo z1, Complejo z2) {
  return (z1 * z2) / mcd(z1, z2);
}

// Definimos una función que devuelve el anillo de los números enteros gaussianos
Set<Complejo> enterosGaussianos() {
  Set<Complejo> enterosGaussianos = HashSet();
  for (int a = -10; a <= 10; a++) {
    for (int b = -10; b <= 10; b++) {
      enterosGaussian