```dart
// Importamos las librerías necesarias
import 'dart:io';
import 'dart:math';
import 'dart:async';
import 'dart:convert';

// Creamos una función para generar un número aleatorio
int generarNumeroAleatorio() {
  Random random = new Random();
  return random.nextInt(100);
}

// Creamos una función para pedir un número al usuario
int pedirNumeroAlUsuario() {
  print('Introduce un número:');
  String? input = stdin.readLineSync();
  return int.parse(input!);
}

// Creamos una función para comprobar si un número es primo
bool esPrimo(int numero) {
  if (numero <= 1) {
    return false;
  }
  for (int i = 2; i < numero; i++) {
    if (numero % i == 0) {
      return false;
    }
  }
  return true;
}

// Creamos una función para encontrar el máximo común divisor de dos números
int maximoComunDivisor(int a, int b) {
  while (b != 0) {
    int temp = b;
    b = a % b;
    a = temp;
  }
  return abs(a);
}

// Creamos una función para encontrar el mínimo común múltiplo de dos números
int minimoComunMultiplo(int a, int b) {
  return (a * b) ~/ maximoComunDivisor(a, b);
}

// Creamos una función para factorizar un número
List<int> factorizarNumero(int numero) {
  List<int> factores = [];
  for (int i = 2; i <= numero; i++) {
    while (numero % i == 0) {
      factores.add(i);
      numero ~/= i;
    }
  }
  return factores;
}

// Creamos una función para convertir un número a binario
String convertirABinario(int numero) {
  String binario = "";
  while (numero > 0) {
    binario = (numero % 2).toString() + binario;
    numero ~/= 2;
  }
  return binario;
}

// Creamos una función para convertir un número a hexadecimal
String convertirAHexadecimal(int numero) {
  String hexadecimal = "";
  while (numero > 0) {
    int digito = numero % 16;
    String digitoHexadecimal = digito < 10 ? digito.toString() : String.fromCharCode(digito + 87);
    hexadecimal = digitoHexadecimal + hexadecimal;
    numero ~/= 16;
  }
  return hexadecimal;
}

// Creamos una función para calcular el factorial de un número
int factorial(int numero) {
  if (numero == 0) {
    return 1;
  }
  return numero * factorial(numero - 1);
}

// Creamos una función para calcular la suma de los dígitos de un número
int sumaDeDigitos(int numero) {
  int suma = 0;
  while (numero > 0) {
    suma += numero % 10;
    numero ~/= 10;
  }
  return suma;
}

// Creamos una función para calcular el promedio de los dígitos de un número
double promedioDeDigitos(int numero) {
  int suma = 0;
  int contador = 0;
  while (numero > 0) {
    suma += numero % 10;
    contador++;
    numero ~/= 10;
  }
  return suma / contador;
}

// Creamos una función para invertir un número
int invertirNumero(int numero) {
  int invertido = 0;
  while (numero > 0) {
    invertido = invertido * 10 + numero % 10;
    numero ~/= 10;
  }
  return invertido;
}

// Creamos una función para comprobar si un número es palíndromo
bool esPalindromo(int numero) {
  return numero == invertirNumero(numero);
}

// Creamos una función para encontrar el número más grande en una lista
int encontrarNumeroMasGrande(List<int> numeros) {
  int numeroMasGrande = numeros[0];
  for (int i = 1; i < numeros.length; i++) {
    if (numeros[i] > numeroMasGrande) {
      numeroMasGrande = numeros[i];
    }
  }
  return numeroMasGrande;
}

// Creamos una función para encontrar el número más pequeño en una lista
int encontrarNumeroMasP