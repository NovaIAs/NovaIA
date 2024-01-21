```dart
// Importación de bibliotecas
import 'dart:async';
import 'dart:collection';
import 'dart:convert';
import 'dart:io';
import 'dart:math';
import 'dart:typed_data';

// Función para generar un número aleatorio entre dos valores
int generarNumeroAleatorio(int minimo, int maximo) {
  Random random = Random();
  return random.nextInt(maximo - minimo) + minimo;
}

// Función para generar una lista de números aleatorios
List<int> generarListaNumerosAleatorios(int cantidad, int minimo, int maximo) {
  List<int> listaNumerosAleatorios = [];
  for (int i = 0; i < cantidad; i++) {
    listaNumerosAleatorios.add(generarNumeroAleatorio(minimo, maximo));
  }
  return listaNumerosAleatorios;
}

// Función para calcular el promedio de una lista de números
double calcularPromedio(List<int> listaNumeros) {
  int suma = 0;
  for (int numero in listaNumeros) {
    suma += numero;
  }
  return suma / listaNumeros.length;
}

// Función para ordenar una lista de números en orden ascendente
List<int> ordenarListaAscendente(List<int> listaNumeros) {
  listaNumeros.sort((a, b) => a.compareTo(b));
  return listaNumeros;
}

// Función para ordenar una lista de números en orden descendente
List<int> ordenarListaDescendente(List<int> listaNumeros) {
  listaNumeros.sort((a, b) => b.compareTo(a));
  return listaNumeros;
}

// Función para buscar un número en una lista y devolver su índice
int buscarNumero(List<int> listaNumeros, int numero) {
  int indice = listaNumeros.indexOf(numero);
  return indice;
}

// Función para eliminar un número de una lista
void eliminarNumero(List<int> listaNumeros, int numero) {
  listaNumeros.remove(numero);
}

// Función para añadir un número a una lista
void añadirNumero(List<int> listaNumeros, int numero) {
  listaNumeros.add(numero);
}

// Función para invertir una lista de números
List<int> invertirLista(List<int> listaNumeros) {
  List<int> listaInvertida = [];
  for (int i = listaNumeros.length - 1; i >= 0; i--) {
    listaInvertida.add(listaNumeros[i]);
  }
  return listaInvertida;
}

// Función para generar una lista de cadenas de caracteres aleatorias
List<String> generarListaCadenasAleatorias(int cantidad, int longitud) {
  List<String> listaCadenasAleatorias = [];
  for (int i = 0; i < cantidad; i++) {
    String cadenaAleatoria = "";
    for (int j = 0; j < longitud; j++) {
      cadenaAleatoria += String.fromCharCode(generarNumeroAleatorio(97, 123));
    }
    listaCadenasAleatorias.add(cadenaAleatoria);
  }
  return listaCadenasAleatorias;
}

// Función para ordenar una lista de cadenas de caracteres en orden ascendente
List<String> ordenarListaCadenasAscendente(List<String> listaCadenas) {
  listaCadenas.sort((a, b) => a.compareTo(b));
  return listaCadenas;
}

// Función para ordenar una lista de cadenas de caracteres en orden descendente
List<String> ordenarListaCadenasDescendente(List<String> listaCadenas) {
  listaCadenas.sort((a, b) => b.compareTo(a));
  return listaCadenas;
}

// Función para buscar una cadena de caracteres en una lista y devolver su índice
int buscarCadena(List<String> listaCadenas, String cadena) {
  int indice = listaCadenas.indexOf(cadena);
  return indice;
}

// Función para eliminar una cadena de caracteres de una lista
void eliminarCadena(List<String> listaCadenas, String cadena) {
  listaCadenas.remove(cadena);
}

// Función para añadir una cadena de caracteres a una lista
void añadirCadena(List<String> listaCadenas, String cadena) {
  listaCadenas.add(cadena);
}

// Función para invertir una lista de cadenas de caracteres
List<String> invertirListaCadenas(List<String> listaCadenas) {
  List<String> listaInvertida = [];
  for (int i = listaCadenas.length - 1; i >= 0; i--) {
    listaInvertida.add(listaCadenas[i]);
  }
  return listaInvertida;
}

// Función para generar un mapa de pares clave-valor aleatorios
Map<String, int> generarMapaAleatorio(int cantidad) {
  Map<String, int> mapaAleatorio = {};
  for (int i = 0; i < cantidad; i++) {
    String claveAleatoria = String.fromCharCode(generarNumeroAleatorio(97, 123));
    int valorAleatorio = generarNumeroAleatorio(0, 100);
    mapaAleatorio[claveAleatoria] = valorAleatorio;
  }
  return mapaAleatorio;
}

// Función para ordenar un mapa por sus claves en orden ascendente
Map<String, int> ordenarMapaAscendente(Map<String, int> mapa) {
  LinkedHashMap<String, int> mapaOrdenado = LinkedHashMap();
  List<String> clavesOrdenadas = mapa.keys.toList()..sort();
  for (String clave in clavesOrdenadas) {