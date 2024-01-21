```dart
import 'dart:io';

/// Esta función toma un número entero y devuelve su factorial.
int factorial(int n) {
  if (n == 0) {
    return 1;
  } else {
    return n * factorial(n - 1);
  }
}

/// Esta función toma una cadena de caracteres y devuelve su longitud.
int longitud(String cadena) {
  return cadena.length;
}

/// Esta función toma una lista de números enteros y devuelve su suma.
int suma(List<int> lista) {
  int suma = 0;
  for (int i = 0; i < lista.length; i++) {
    suma += lista[i];
  }
  return suma;
}

/// Esta función toma un archivo y devuelve su contenido.
String leerArchivo(File archivo) {
  return archivo.readAsStringSync();
}

/// Esta función toma un archivo y escribe una cadena de caracteres en él.
void escribirArchivo(File archivo, String contenido) {
  archivo.writeAsStringSync(contenido);
}

/// Esta función toma una lista de cadenas de caracteres y devuelve una nueva lista con todas las cadenas ordenadas alfabéticamente.
List<String> ordenarLista(List<String> lista) {
  lista.sort();
  return lista;
}

/// Esta función toma una lista de números enteros y devuelve una nueva lista con todos los números ordenados de menor a mayor.
List<int> ordenarListaNumeros(List<int> lista) {
  lista.sort((a, b) => a.compareTo(b));
  return lista;
}

/// Esta función toma una lista de números enteros y devuelve una nueva lista con todos los números que son pares.
List<int> filtrarListaPares(List<int> lista) {
  return lista.where((numero) => numero % 2 == 0).toList();
}

/// Esta función toma una lista de números enteros y devuelve una nueva lista con todos los números que son impares.
List<int> filtrarListaImpares(List<int> lista) {
  return lista.where((numero) => numero % 2 != 0).toList();
}

/// Esta función toma una lista de números enteros y devuelve una nueva lista con todos los números que son mayores que un valor dado.
List<int> filtrarListaMayoresQue(List<int> lista, int valor) {
  return lista.where((numero) => numero > valor).toList();
}

/// Esta función toma una lista de números enteros y devuelve una nueva lista con todos los números que son menores que un valor dado.
List<int> filtrarListaMenoresQue(List<int> lista, int valor) {
  return lista.where((numero) => numero < valor).toList();
}

/// Esta función toma una lista de números enteros y devuelve una nueva lista con todos los números que están entre dos valores dados.
List<int> filtrarListaEntre(List<int> lista, int valor1, int valor2) {
  return lista.where((numero) => numero > valor1 && numero < valor2).toList();
}

/// Esta función toma una lista de números enteros y devuelve una nueva lista con todos los números que son múltiplos de un valor dado.
List<int> filtrarListaMultiplosDe(List<int> lista, int valor) {
  return lista.where((numero) => numero % valor == 0).toList();
}

/// Esta función toma una lista de números enteros y devuelve una nueva lista con todos los números que son divisores de un valor dado.
List<int> filtrarListaDivisoresDe(List<int> lista, int valor) {
  return lista.where((numero) => valor % numero == 0).toList();
}

/// Esta función toma una lista de números enteros y devuelve una nueva lista con todos los números que son primos.
List<int> filtrarListaPrimos(List<int> lista) {
  return lista
      .where((numero) => numero > 1 &&
          !(lista.where((divisor) => numero % divisor == 0).toList().length > 2))
      .toList();
}

/// Esta función toma una lista de números enteros y devuelve una nueva lista con todos los números que son perfectos.
List<int> filtrarListaPerfectos(List<int> lista) {
  return lista
      .where((numero) =>
          numero ==
          lista
              .where((divisor) => numero % divisor == 0)
              .toList()
              .reduce((a, b) => a + b) -
              numero)
      .toList();
}

/// Esta función toma una lista de números enteros y devuelve una nueva lista con todos los números que son abundantes.
List<int> filtrarListaAbundantes(List<int> lista) {
  return lista
      .where((numero) =>
          numero <
          lista
              .where((divisor) => numero % divisor == 0)
              .toList()
              .reduce((a, b) => a + b) -
              numero)
      .toList();
}

/// Esta función toma una lista de números enteros y devuelve una nueva lista con todos los números que son deficientes.
List<int> filtrarListaDeficientes(List<int> lista) {
  return lista
      .where((numero) =>
          numero >
          lista
              .where((divisor) => numero % divisor == 0)
              .toList()
              .reduce((a, b) => a + b) -
              numero)
      .toList();
}

/// Esta función toma una lista de números enteros y devuelve una nueva lista con todos los números que son amigables.
List<int> filtrarListaAmigables(List<int> lista) {
  return lista
      .where((numero) =>
          lista
              .where((divisor) => numero % divisor == 0)
              .toList()
              .reduce((a, b) => a + b) -
              numero ==
          lista
              .where((divisor) => (numero + 1) % divisor == 0)
              .toList()
              .reduce((a, b) => a + b) -
              (numero + 1))
      .toList();
}

/// Esta función toma una lista de números enteros y devuelve una nueva lista con todos los números que son sociables.
List<int> filtrarListaSociables(List