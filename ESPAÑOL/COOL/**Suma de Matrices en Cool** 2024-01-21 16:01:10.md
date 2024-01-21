```cool
-- Programa SumaMatrices
-- Este programa suma dos matrices de enteros y muestra el resultado.

-- Se definen los tipos de datos.
type Int = Int64;
type Matrix = Array[Int, Int];

-- Se definen las funciones.
proc Main() {
  var a: Matrix := { {1, 2, 3}, {4, 5, 6}, {7, 8, 9} };
  var b: Matrix := { {10, 11, 12}, {13, 14, 15}, {16, 17, 18} };
  var c: Matrix := NewArray[Int, Int](3, 3);

  -- Se suman las matrices.
  for i in 0 .. 2 {
    for j in 0 .. 2 {
      c[i][j] := a[i][j] + b[i][j];
    }
  }

  -- Se muestran las matrices.
  Print("Matriz A:\n");
  for i in 0 .. 2 {
    for j in 0 .. 2 {
      Print("%d ", a[i][j]);
    }
    Print("\n");
  }

  Print("Matriz B:\n");
  for i in 0 .. 2 {
    for j in 0 .. 2 {
      Print("%d ", b[i][j]);
    }
    Print("\n");
  }

  Print("Matriz Suma:\n");
  for i in 0 .. 2 {
    for j in 0 .. 2 {
      Print("%d ", c[i][j]);
    }
    Print("\n");
  }
}
```

Este programa suma dos matrices de enteros y muestra el resultado.

El programa comienza definiendo los tipos de datos. El tipo `Int` es un entero de 64 bits. El tipo `Matrix` es una matriz de enteros.

A continuación, el programa define las funciones. La función `Main` es la función principal del programa. La función `NewArray` crea una nueva matriz. La función `Print` imprime un mensaje en la consola.

En la función `Main`, el programa crea dos matrices, `a` y `b`, con los siguientes valores:

```
a = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}
b = {{10, 11, 12}, {13, 14, 15}, {16, 17, 18}}
```

A continuación, el programa crea una nueva matriz, `c`, del mismo tamaño que `a` y `b`.

Luego, el programa suma las matrices `a` y `b` y almacena el resultado en la matriz `c`.

Finalmente, el programa muestra las matrices `a`, `b` y `c` en la consola.

Este programa es un ejemplo de cómo utilizar los tipos de datos y las funciones en Cool. El programa es complejo porque utiliza múltiples matrices y loops para sumar las matrices. El programa también utiliza la función `Print` para mostrar los resultados en la consola.