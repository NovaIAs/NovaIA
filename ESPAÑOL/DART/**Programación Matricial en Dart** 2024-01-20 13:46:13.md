```dart
import 'dart:io';
import 'dart:math';

// Clase que representa una matriz
class Matriz {
  List<List<double>> _matriz; // Matriz interna
  int _filas; // Número de filas de la matriz
  int _columnas; // Número de columnas de la matriz

  // Constructor
  Matriz(this._filas, this._columnas) {
    _matriz = List.generate(_filas, (i) => List.filled(_columnas, 0.0));
  }

  // Obtener el número de filas de la matriz
  int get filas => _filas;

  // Obtener el número de columnas de la matriz
  int get columnas => _columnas;

  // Obtener un elemento de la matriz
  double get(int fila, int columna) {
    return _matriz[fila][columna];
  }

  // Establecer un elemento de la matriz
  void set(int fila, int columna, double valor) {
    _matriz[fila][columna] = valor;
  }

  // Sumar dos matrices
  Matriz operator +(Matriz otraMatriz) {
    if (_filas != otraMatriz._filas || _columnas != otraMatriz._columnas) {
      throw ArgumentError("Las matrices no tienen el mismo tamaño");
    }
    Matriz resultado = Matriz(_filas, _columnas);
    for (int i = 0; i < _filas; i++) {
      for (int j = 0; j < _columnas; j++) {
        resultado._matriz[i][j] = _matriz[i][j] + otraMatriz._matriz[i][j];
      }
    }
    return resultado;
  }

  // Restar dos matrices
  Matriz operator -(Matriz otraMatriz) {
    if (_filas != otraMatriz._filas || _columnas != otraMatriz._columnas) {
      throw ArgumentError("Las matrices no tienen el mismo tamaño");
    }
    Matriz resultado = Matriz(_filas, _columnas);
    for (int i = 0; i < _filas; i++) {
      for (int j = 0; j < _columnas; j++) {
        resultado._matriz[i][j] = _matriz[i][j] - otraMatriz._matriz[i][j];
      }
    }
    return resultado;
  }

  // Multiplicar dos matrices
  Matriz operator *(Matriz otraMatriz) {
    if (_columnas != otraMatriz._filas) {
      throw ArgumentError("El número de columnas de la primera matriz debe ser igual al número de filas de la segunda matriz");
    }
    Matriz resultado = Matriz(_filas, otraMatriz._columnas);
    for (int i = 0; i < _filas; i++) {
      for (int j = 0; j < otraMatriz._columnas; j++) {
        double suma = 0.0;
        for (int k = 0; k < _columnas; k++) {
          suma += _matriz[i][k] * otraMatriz._matriz[k][j];
        }
        resultado._matriz[i][j] = suma;
      }
    }
    return resultado;
  }

  // Imprimir la matriz
  void imprimir() {
    for (int i = 0; i < _filas; i++) {
      for (int j = 0; j < _columnas; j++) {
        stdout.write("${_matriz[i][j]} ");
      }
      stdout.writeln();
    }
  }

  // Determinante de una matriz
  double determinante() {
    if (_filas != _columnas) {
      throw ArgumentError("La matriz no es cuadrada");
    }
    if (_filas == 1) {
      return _matriz[0][0];
    } else if (_filas == 2) {
      return _matriz[0][0] * _matriz[1][1] - _matriz[0][1] * _matriz[1][0];
    } else {
      double det = 0.0;
      for (int i = 0; i < _columnas; i++) {
        Matriz subMatriz = Matriz(_filas - 1, _columnas - 1);
        for (int j = 1; j < _filas; j++) {
          int k = 0;
          for (int l = 0; l < _columnas; l++) {
            if (l != i) {
              subMatriz._matriz[j - 1][k] = _matriz[j][l];
              k++;
            }
          }
        }
        det += pow(-1, i) * _matriz[0][i] * subMatriz.determinante();
      }
      return det;