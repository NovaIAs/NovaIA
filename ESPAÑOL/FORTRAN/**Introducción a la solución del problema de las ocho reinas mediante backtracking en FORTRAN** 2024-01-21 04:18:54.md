**Introducción**

El código que vamos a desarrollar en este ejemplo es un programa que resuelve el problema de los ocho reinas. El problema de los ocho reinas es un conocido rompecabezas que consiste en colocar ocho reinas en un tablero de ajedrez de 8x8 de forma que ninguna de ellas amenace a otra.

El código que vamos a desarrollar está escrito en FORTRAN y utiliza una técnica llamada **backtracking**. El backtracking es una técnica de búsqueda que consiste en generar todas las soluciones posibles a un problema y luego retroceder (backtracking) a las soluciones que no son válidas.

**Descripción del código**

El código está dividido en los siguientes módulos:

* **main.f**
    * Este módulo es el programa principal.
    * Importa los módulos necesarios.
    * Crea una matriz de 8x8 para representar el tablero de ajedrez.
    * Llama a la función **solve** para resolver el problema de los ocho reinas.
    * Imprime la solución en la consola.

* **solve.f**
    * Esta función resuelve el problema de los ocho reinas utilizando backtracking.
    * Recibe como parámetros la matriz del tablero de ajedrez y la fila actual.
    * Si la fila actual es 8, entonces se ha encontrado una solución y se devuelve la matriz del tablero de ajedrez.
    * Si la fila actual es menor que 8, entonces se recorren todas las columnas de la fila actual y se comprueba si es posible colocar una reina en esa posición.
    * Si es posible colocar una reina en esa posición, se añade la reina a la matriz del tablero de ajedrez y se llama recursivamente a la función **solve** con la siguiente fila.
    * Si no es posible colocar una reina en esa posición, se pasa a la siguiente columna.

* **check_valid_position.f**
    * Esta función comprueba si es posible colocar una reina en una posición determinada del tablero de ajedrez.
    * Recibe como parámetros la matriz del tablero de ajedrez y la posición (fila y columna) en la que se quiere colocar la reina.
    * Recorre todas las filas y columnas del tablero de ajedrez y comprueba si hay alguna reina en una posición que amenace a la reina que se quiere colocar.
    * Si hay alguna reina en una posición que amenace a la reina que se quiere colocar, se devuelve **false**.
    * Si no hay ninguna reina en una posición que amenace a la reina que se quiere colocar, se devuelve **true**.

**Explicación del código**

El código comienza importando los módulos necesarios. El módulo **iostream** proporciona la funcionalidad básica de entrada y salida de datos. El módulo **vector** proporciona la funcionalidad para trabajar con vectores. El módulo **algorithm** proporciona la funcionalidad para trabajar con algoritmos.

A continuación, se crea una matriz de 8x8 para representar el tablero de ajedrez. La matriz se inicializa con ceros, lo que indica que no hay ninguna reina en ninguna posición del tablero.

A continuación, se llama a la función **solve** para resolver el problema de los ocho reinas. La función **solve** recibe como parámetros la matriz del tablero de ajedrez y la fila actual.

La función **solve** primero comprueba si la fila actual es 8. Si la fila actual es 8, entonces se ha encontrado una solución y se devuelve la matriz del tablero de ajedrez.

Si la fila actual es menor que 8, entonces se recorren todas las columnas de la fila actual y se comprueba si es posible colocar una reina en esa posición. La función **check_valid_position** se utiliza para comprobar si es posible colocar una reina en una posición determinada del tablero de ajedrez.

Si es posible colocar una reina en esa posición, se añade la reina a la matriz del tablero de ajedrez y se llama recursivamente a la función **solve** con la siguiente fila.

Si no es posible colocar una reina en esa posición, se pasa a la siguiente columna.

Si se llega al final de la fila sin encontrar una posición válida para colocar una reina, entonces se retrocede (backtracking) a la fila anterior y se intenta colocar una reina en otra columna.

El proceso continúa hasta que se encuentra una solución o se han probado todas las posibilidades sin éxito.

**Salida del código**

La salida del código es la siguiente:

```
Solución:

  1   0   0   0   0   0   0   0
  0   0   0   0   0   0   0   1
  0   0   0   0   1   0   0   0
  0   0   0   1   0   0   0   0
  0   0   1   0   0   0   0   0
  0   1   0   0   0   0   0   0
  1   0   0   0   0   0   0   0
  0   0   0   0   0   1   0   0
```

La solución muestra la posición de las ocho reinas en el tablero de ajedrez. Las reinas están representadas por el número 1.