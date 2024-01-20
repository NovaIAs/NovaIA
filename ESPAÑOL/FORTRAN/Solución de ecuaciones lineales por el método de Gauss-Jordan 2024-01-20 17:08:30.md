```
PROGRAMA PRINCIPAL

IMPRIMIR "Solución de ecuaciones lineales por el método de Gauss-Jordan"

DECLARAR VARIABLES
  REAL A(3, 3) 'Matriz de coeficientes'
  REAL B(3) 'Vector de términos independientes'
  REAL X(3) 'Vector de incógnitas'
  REAL MAX 'Valor máximo'
  ENTERO I, J, K 'Índices'

LEER "Ingrese los coeficientes de la matriz A:"
  DO I = 1 A 3
    DO J = 1 A 3
      LEER A(I, J)
    END DO
  END DO

LEER "Ingrese los términos independientes del vector B:"
  DO I = 1 A 3
    LEER B(I)
  END DO

IMPRIMIR "Matriz A original:"
  DO I = 1 A 3
    DO J = 1 A 3
      IMPRIMIR A(I, J), ""
    END DO
    IMPRIMIR
  END DO

IMPRIMIR "Vector B original:"
  DO I = 1 A 3
    IMPRIMIR B(I), ""
  END DO
  IMPRIMIR

IMPRIMIR "Matriz A aumentada:"
  DO I = 1 A 3
    DO J = 1 A 4
      SI J <= 3 ENTONCES
        IMPRIMIR A(I, J), ""
      SINO
        IMPRIMIR B(I - 1), ""
      FIN SI
    END DO
    IMPRIMIR
  END DO

DO K = 1 A 3
  'Encontrar el pivote en la columna K'
  MAX = ABS(A(K, K))
  I = K
  DO J = K + 1 A 3
    SI ABS(A(J, K)) > MAX ENTONCES
      MAX = ABS(A(J, K))
      I = J
    FIN SI
  END DO

  'Intercambiar filas K e I si es necesario'
  SI K <> I ENTONCES
    DO J = 1 A 4
      R = A(K, J)
      A(K, J) = A(I, J)
      A(I, J) = R
    END DO
  FIN SI

  'Hacer ceros por encima y por debajo del pivote'
  DO I = 1 A 3
    SI I <> K ENTONCES
      M = -A(I, K) / A(K, K)
      DO J = 1 A 4
        A(I, J) = A(I, J) + M * A(K, J)
        B(I) = B(I) + M * B(K)
      END DO
    FIN SI
  END DO
END DO

'Impresión de la matriz A aumentada después del proceso de Gauss-Jordan'
IMPRIMIR "Matriz A aumentada después del proceso de Gauss-Jordan:"
  DO I = 1 A 3
    DO J = 1 A 4
      SI J <= 3 ENTONCES
        IMPRIMIR A(I, J), ""
      SINO
        IMPRIMIR B(I - 1), ""
      FIN SI
    END DO
    IMPRIMIR
  END DO

'Extracción de las incógnitas'
DO I = 1 A 3
  X(I) = B(I) / A(I, I)
END DO

'Impresión del vector de incógnitas'
IMPRIMIR "Vector de incógnitas:"
  DO I = 1 A 3
    IMPRIMIR X(I), ""
  END DO
  IMPRIMIR

FIN PROGRAMA
```

Explicación del código:

Este código implementa el método de Gauss-Jordan para resolver un sistema de ecuaciones lineales de tres incógnitas y tres ecuaciones.

* Primero, se declaran las variables necesarias:
    * `A`: matriz de coeficientes del sistema de ecuaciones.
    * `B`: vector de términos independientes del sistema de ecuaciones.
    * `X`: vector de incógnitas del sistema de ecuaciones.
    * `MAX`: valor máximo utilizado para encontrar el pivote en cada columna.
    * `I`, `J`, `K`: índices utilizados para recorrer las filas y columnas de la matriz.

* A continuación, se leen los coeficientes de la matriz `A` y los términos independientes del vector `B` desde la entrada estándar.

* Se imprime la matriz `A` original y el vector `B` original.

* Se crea una matriz aumentada `A`, que es la matriz `A` con el vector `B` como última columna.

* Se inicia un bucle `DO` para recorrer las columnas de la matriz aumentada.

    * En cada paso del bucle, se encuentra el pivote en la columna actual `K`.
    * Se intercambian las filas `K` e `I` si es necesario para colocar el pivote en la posición `(K, K)`.
    * Se hacen ceros por encima y por debajo del pivote utilizando operaciones de fila elementales.

* Después del bucle `DO`, la matriz aumentada está en forma de identidad. Esto significa que las incógnitas pueden extraerse fácilmente de la última columna de la matriz aumentada.

* Se imprimen la matriz aumentada después del proceso de Gauss-Jordan y el vector de incógnitas `X`.

* Finalmente, el programa termina.