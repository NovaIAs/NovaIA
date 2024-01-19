```dart
import 'dart:math';

class Complejo {
  // Atributos
  double real;
  double imaginaria;

  // Constructores
  Complejo(this.real, this.imaginaria);
  Complejo.desdePolar(double modulo, double argumento) {
    real = modulo * cos(argumento);
    imaginaria = modulo * sin(argumento);
  }

  // Métodos
  double modulo() {
    return sqrt(real * real + imaginaria * imaginaria);
  }

  double argumento() {
    return atan2(imaginaria, real);
  }

  Complejo conjugado() {
    return Complejo(real, -imaginaria);
  }

  Complejo sumar(Complejo otro) {
    return Complejo(real + otro.real, imaginaria + otro.imaginaria);
  }

  Complejo restar(Complejo otro) {
    return Complejo(real - otro.real, imaginaria - otro.imaginaria);
  }

  Complejo multiplicar(Complejo otro) {
    return Complejo(
        real * otro.real - imaginaria * otro.imaginaria,
        real * otro.imaginaria + imaginaria * otro.real);
  }

  Complejo dividir(Complejo otro) {
    double denominador = otro.real * otro.real + otro.imaginaria * otro.imaginaria;
    return Complejo(
        (real * otro.real + imaginaria * otro.imaginaria) / denominador,
        (imaginaria * otro.real - real * otro.imaginaria) / denominador);
  }

  // Sobrecargas de operadores
  operator +(Complejo otro) => sumar(otro);
  operator -(Complejo otro) => restar(otro);
  operator *(Complejo otro) => multiplicar(otro);
  operator /(Complejo otro) => dividir(otro);

  // Métodos estáticos
  static Complejo desdeString(String string) {
    var partes = string.split('+');
    return Complejo(double.parse(partes[0]), double.parse(partes[1].substring(0, partes[1].length - 1)));
  }

  // Override de métodos
  @override
  String toString() {
    return '$real + ${imaginaria}i';
  }
}

// Función principal
void main() {
  // Creación de objetos complejos
  var c1 = Complejo(3, 4);
  var c2 = Complejo.desdePolar(5, pi / 4);

  // Operaciones con objetos complejos
  var suma = c1 + c2;
  var resta = c1 - c2;
  var producto = c1 * c2;
  var cociente = c1 / c2;

  // Impresión de resultados
  print('Suma: $suma');
  print('Resta: $resta');
  print('Producto: $producto');
  print('Cociente: $cociente');
}
```

Explicación del código:

* La clase `Complejo` representa un número complejo con atributos `real` e `imaginaria`.
* El constructor `Complejo` crea un número complejo a partir de partes reales e imaginarias.
* El constructor `Complejo.desdePolar` crea un número complejo a partir de un módulo y un argumento.
* Los métodos `modulo()` y `argumento()` calculan el módulo y el argumento del número complejo, respectivamente.
* El método `conjugado()` devuelve el conjugado del número complejo.
* Los métodos `sumar()`, `restar()`, `multiplicar()` y `dividir()` realizan las operaciones aritméticas básicas con números complejos.
* Los operadores `+`, `-`, `*` y `/` están sobrecargados para permitir operaciones aritméticas con números complejos.
* El método `desdeString()` crea un número complejo a partir de una cadena de texto.
* El método `toString()` devuelve una cadena de texto que representa el número complejo.
* La función `main()` crea dos objetos complejos y realiza operaciones aritméticas con ellos.