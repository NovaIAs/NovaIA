```dart
// Importamos la librería de dart:io para poder utilizar la función print()
import 'dart:io';

// Creamos una función recursiva llamada factorial que calcula el factorial de un número
int factorial(int numero) {
  // Si el número es 0, retornamos 1 (caso base)
  if (numero == 0) {
    return 1;
  }
  // Si el número es positivo, lo multiplicamos por el factorial del número menos 1
  else if (numero > 0) {
    return numero * factorial(numero - 1);
  }
  // Si el número es negativo, lanzamos una excepción
  else {
    throw ArgumentError("El número debe ser positivo o cero");
  }
}

// Creamos una función recursiva llamada fibonacci que calcula el número de Fibonacci de un índice
int fibonacci(int indice) {
  // Si el índice es 0 o 1, retornamos el índice (caso base)
  if (indice <= 1) {
    return indice;
  }
  // Si el índice es mayor que 1, calculamos el número de Fibonacci del índice menos 1 y 2, y los sumamos
  else {
    return fibonacci(indice - 1) + fibonacci(indice - 2);
  }
}

// Creamos una función que calcula el máximo común divisor de dos números utilizando el algoritmo de Euclides
int maximoComunDivisor(int a, int b) {
  // Si b es 0, retornamos a (caso base)
  if (b == 0) {
    return a;
  }
  // Si b es positivo, calculamos el máximo común divisor de b y el resto de a dividido por b
  else {
    return maximoComunDivisor(b, a % b);
  }
}

// Creamos una función que calcula el mínimo común múltiplo de dos números
int minimoComunMultiplo(int a, int b) {
  // Calculamos el máximo común divisor de a y b
  int mcd = maximoComunDivisor(a, b);
  // Calculamos el mínimo común múltiplo de a y b utilizando la fórmula mcm = (a * b) / mcd
  return (a * b) ~/ mcd;
}

// Creamos una función que calcula el menor múltiplo común de un array de números
int menorMultiploComun(List<int> numeros) {
  // Inicializamos el menor múltiplo común con el primer número del array
  int mmc = numeros[0];
  // Recorremos el resto de los números del array
  for (int i = 1; i < numeros.length; i++) {
    // Calculamos el mínimo común múltiplo del menor múltiplo común actual y el número actual
    mmc = minimoComunMultiplo(mmc, numeros[i]);
  }
  // Retornamos el menor múltiplo común
  return mmc;
}

// Creamos una función que comprueba si un número es primo
bool esPrimo(int numero) {
  // Si el número es 0 o 1, no es primo
  if (numero <= 1) {
    return false;
  }
  // Recorremos los números desde 2 hasta la raíz cuadrada del número
  for (int i = 2; i <= Math.sqrt(numero).toInt(); i++) {
    // Si el número es divisible por alguno de los números desde 2 hasta su raíz cuadrada, no es primo
    if (numero % i == 0) {
      return false;
    }
  }
  // Si el número no es divisible por ninguno de los números desde 2 hasta su raíz cuadrada, es primo
  return true;
}

// Creamos una función que calcula la suma de los dígitos de un número
int sumaDeDigitos(int numero) {
  // Inicializamos la suma de los dígitos con 0
  int suma = 0;
  // Recorremos los dígitos del número
  while (numero > 0) {
    // Añadimos el último dígito del número a la suma
    suma += numero % 10;
    // Eliminamos el último dígito del número
    numero ~/= 10;
  }
  // Retornamos la suma de los dígitos
  return suma;
}

// Creamos una función que imprime el resultado de una lista de funciones pasadas como argumento
void imprimirResultados(List<Function> funciones) {
  // Recorremos la lista de funciones
  for (Function funcion in funciones) {
    // Imprimimos el resultado de la función
    print(funcion());
  }
}

// Creamos una lista de funciones que queremos ejecutar
List<Function> funciones = [
  () => factorial(5),
  () => fibonacci(10),
  () => maximoComunDivisor(24, 18),
  () => minimoComunMultiplo(12, 15),
  () => menorMultiploComun([2, 3, 4, 5, 6]),
  () => esPrimo(17),
  () => sumaDeDigitos(12345)
];

// Imprimimos los resultados de las funciones
imprimirResultados(funciones);
```

Explicación del código:

* La función `factorial` calcula el factorial de un número utilizando recursividad.
* La función `fibonacci` calcula el número de Fibonacci de un índice utilizando recursividad.
* La función `maximoComunDivisor` calcula el máximo común divisor de dos números utilizando el algoritmo de Euclides.
* La función `minimoComunMultiplo` calcula el mínimo común múltiplo de dos números.
* La función `menorMultiploComun` calcula el menor múltiplo común de un array de números.
* La función `esPrimo` comprueba si un número es primo.
* La función `sumaDeDigitos` calcula la suma de los dígitos de un número.
* La función `imprimirResultados` imprime el resultado de una lista de funciones pasadas como argumento.

En la última línea del código, creamos una lista de funciones que queremos ejecutar y llamamos a la función `imprimirResultados` para imprimir los resultados de las funciones.