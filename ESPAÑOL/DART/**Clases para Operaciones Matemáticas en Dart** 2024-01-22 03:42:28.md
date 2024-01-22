```dart
// Clase "Fraccion" para representar fracciones matemáticas.
class Fraccion {
  int numerador;
  int denominador;

  Fraccion(this.numerador, this.denominador) {
    // Simplificar la fracción
    int mcd = calcularMCD(numerador, denominador);
    numerador ~/= mcd;
    denominador ~/= mcd;
  }

  // Calcular el máximo común divisor (MCD) de dos números.
  int calcularMCD(int num1, int num2) {
    if (num2 == 0) {
      return num1;
    } else {
      return calcularMCD(num2, num1 % num2);
    }
  }

  // Sumar dos fracciones.
  Fraccion suma(Fraccion fraccion) {
    return Fraccion(
        numerador * fraccion.denominador + fraccion.numerador * denominador,
        denominador * fraccion.denominador);
  }

  // Restar dos fracciones.
  Fraccion resta(Fraccion fraccion) {
    return Fraccion(
        numerador * fraccion.denominador - fraccion.numerador * denominador,
        denominador * fraccion.denominador);
  }

  // Multiplicar dos fracciones.
  Fraccion multiplica(Fraccion fraccion) {
    return Fraccion(numerador * fraccion.numerador, denominador * fraccion.denominador);
  }

  // Dividir dos fracciones.
  Fraccion divide(Fraccion fraccion) {
    return Fraccion(numerador * fraccion.denominador, denominador * fraccion.numerador);
  }

  // Comprobar si dos fracciones son iguales.
  bool esIgual(Fraccion fraccion) {
    return numerador == fraccion.numerador && denominador == fraccion.denominador;
  }

  // Obtener el valor decimal de la fracción.
  double get decimal {
    return numerador / denominador;
  }

  // Representar la fracción en forma de cadena de texto.
  @override
  String toString() {
    return "$numerador/$denominador";
  }
}

// Clase "Complejo" para representar números complejos.
class Complejo {
  double real;
  double imaginario;

  Complejo(this.real, this.imaginario);

  // Sumar dos números complejos.
  Complejo suma(Complejo complejo) {
    return Complejo(real + complejo.real, imaginario + complejo.imaginario);
  }

  // Restar dos números complejos.
  Complejo resta(Complejo complejo) {
    return Complejo(real - complejo.real, imaginario - complejo.imaginario);
  }

  // Multiplicar dos números complejos.
  Complejo multiplica(Complejo complejo) {
    return Complejo(
        real * complejo.real - imaginario * complejo.imaginario,
        real * complejo.imaginario + imaginario * complejo.real);
  }

  // Dividir dos números complejos.
  Complejo divide(Complejo complejo) {
    double denominador = complejo.real * complejo.real + complejo.imaginario * complejo.imaginario;
    return Complejo(
        (real * complejo.real + imaginario * complejo.imaginario) / denominador,
        (imaginario * complejo.real - real * complejo.imaginario) / denominador);
  }

  // Comprobar si dos números complejos son iguales.
  bool esIgual(Complejo complejo) {
    return real == complejo.real && imaginario == complejo.imaginario;
  }

  // Obtener el módulo del número complejo.
  double get modulo {
    return sqrt(real * real + imaginario * imaginario);
  }

  // Obtener el argumento del número complejo.
  double get argumento {
    return atan2(imaginario, real);
  }

  // Representar el número complejo en forma de cadena de texto.
  @override
  String toString() {
    return "$real + ${imaginario}i";
  }
}

// Clase "Polinomio" para representar polinomios.
class Polinomio {
  List<double> coeficientes;

  Polinomio(this.coeficientes);

  // Sumar dos polinomios.
  Polinomio suma(Polinomio polinomio) {
    List<double> nuevosCoeficientes = [];
    for (int i = 0; i < max(coeficientes.length, polinomio.coeficientes.length); i++) {
      nuevosCoeficientes.add((coeficientes[i] ?? 0) + (polinomio.coeficientes[i] ?? 0));
    }
    return Polinomio(nuevosCoeficientes);
  }

  // Restar dos polinomios.
  Polinomio resta(Polinomio polinomio) {
    List<double> nuevosCoeficientes = [];
    for (int i = 0; i < max(coeficientes.length, polinomio.coeficientes.length); i++) {
      nuevosCoeficientes.add((coeficientes[i] ?? 0) - (polinomio.coeficientes[i] ?? 0));
    }
    return Polinomio(nuevosCoeficientes);
  }

  // Multiplicar dos polinomios.
  Polinomio multiplica(Polinomio polinomio) {
    List<double> nuevosCoeficientes = [];
    for (int i = 0; i < coeficientes.length + polinomio.coeficientes.length - 1; i++) {
      nuevosCoeficientes.add(0);
    }
    for (int i = 0; i < coeficientes.length; i++) {
      for (int j = 0; j < polinomio.coeficientes.length; j++) {
        nuevosCoeficientes[i + j] += coeficientes[i] * polinomio.coeficientes[j];
      }
    }
    return Polinomio(nuevosCoeficientes);
  }

  // Dividir dos polinomios.
  Polinomio divide(Polinomio polinomio) {
    List<double> cociente = [];
    List<double> residuo = coeficientes;
    while (residuo.length >= polinomio.coeficientes.length) {
      cociente.add(residuo[0] / polinomio.coeficientes[0]);
      residuo = [];
      for (int i = 0; i < residuo.length - polinomio.coeficientes.length + 1; i++) {
        residuo.add(residuo[i] - cociente.last * polinomio.coeficientes[i]);
      }
    }
    return Polinomio(cociente);
  }

  // Evaluar el polinomio en un valor dado.
  double evaluar(double x) {
    double resultado = 0;
    for (int i = 0; i < coeficientes.length; i++) {
      resultado += coeficientes[i] * pow(x, i);
    }
    return resultado;
  }

  // Obtener la derivada del polinomio.
  Polinomio derivada() {
    List<double> nuevosCoeficientes = [];
    for (int i = 1; i < coeficientes.length; i++) {
      nuevosCoeficientes.add(coeficientes[i] * i);
    }
    return Polinomio(nuevosCoeficientes);
  }

  // Obtener la integral del polinomio.
  Polinomio integral() {
    List<double