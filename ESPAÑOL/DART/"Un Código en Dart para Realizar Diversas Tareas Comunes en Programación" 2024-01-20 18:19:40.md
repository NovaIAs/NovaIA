```dart
// Importamos las bibliotecas necesarias
import 'dart:io';
import 'dart:math';

// Creamos una clase llamada Persona
class Persona {
  // Definimos los atributos de la clase
  String nombre;
  int edad;
  double altura;

  // Definimos el constructor de la clase
  Persona(this.nombre, this.edad, this.altura);

  // Definimos los métodos de la clase
  String saludar() {
    return "Hola, mi nombre es $nombre y tengo $edad años.";
  }

  double calcularIMC() {
    return altura / pow(edad, 2);
  }
}

// Creamos una clase llamada Rectangulo
class Rectangulo {
  // Definimos los atributos de la clase
  double ancho;
  double alto;

  // Definimos el constructor de la clase
  Rectangulo(this.ancho, this.alto);

  // Definimos los métodos de la clase
  double calcularArea() {
    return ancho * alto;
  }

  double calcularPerimetro() {
    return 2 * (ancho + alto);
  }
}

// Creamos una clase llamada Circulo
class Circulo {
  // Definimos los atributos de la clase
  double radio;

  // Definimos el constructor de la clase
  Circulo(this.radio);

  // Definimos los métodos de la clase
  double calcularArea() {
    return pi * pow(radio, 2);
  }

  double calcularPerimetro() {
    return 2 * pi * radio;
  }
}

// Creamos una función que recibe una lista de números y devuelve la suma de los mismos
double sumarNumeros(List<double> numeros) {
  double suma = 0;
  for (double numero in numeros) {
    suma += numero;
  }
  return suma;
}

// Creamos una función que recibe una lista de números y devuelve el número mayor de la lista
double encontrarMayor(List<double> numeros) {
  double mayor = numeros[0];
  for (double numero in numeros) {
    if (numero > mayor) {
      mayor = numero;
    }
  }
  return mayor;
}

// Creamos una función que recibe dos números y devuelve la división de los mismos
double dividirNumeros(double dividendo, double divisor) {
  if (divisor == 0) {
    throw Exception("No se puede dividir por cero");
  }
  return dividendo / divisor;
}

// Creamos una función que recibe una cadena de texto y devuelve la cadena de texto invertida
String invertirCadena(String cadena) {
  String cadenaInvertida = "";
  for (int i = cadena.length - 1; i >= 0; i--) {
    cadenaInvertida += cadena[i];
  }
  return cadenaInvertida;
}

// Creamos una función que recibe un número y devuelve el factorial del mismo
int factorial(int numero) {
  if (numero < 0) {
    throw Exception("No se puede calcular el factorial de un número negativo");
  }
  if (numero == 0) {
    return 1;
  }
  return numero * factorial(numero - 1);
}

// Creamos una función que recibe una lista de números y devuelve una lista con los números pares de la lista original
List<int> filtrarPares(List<int> numeros) {
  List<int> pares = [];
  for (int numero in numeros) {
    if (numero % 2 == 0) {
      pares.add(numero);
    }
  }
  return pares;
}

// Creamos una función que recibe una lista de números y devuelve una lista con los números impares