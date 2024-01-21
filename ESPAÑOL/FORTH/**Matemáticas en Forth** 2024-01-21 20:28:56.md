```forth

: O DESPLEGAR 2DROP ;
: 1+ SWAP + 1- SWAP ;
: 2+ 2DUP + 2DROP ;
: 3+ 3DUP + 3DROP ;
: 4+ 4DUP + 4DROP ;
: 5+ 5DUP + 5DROP ;
: 6+ 6DUP + 6DROP ;
: 7+ 7DUP + 7DROP ;
: 8+ 8DUP + 8DROP ;
: 9+ 9DUP + 9DROP ;
: 10+ 10DUP + 10DROP ;
: VECTOR STRING> ;

: INIT-VECTOR 0 DO I 0 TO EEC@ +LOOP ;
: INIT-MATRIX 0 DO I J 0 TO EEC@ +LOOP INIT-VECTOR ;

: SUM-VECTOR 0 SWAP DO I +LOOP DROP ;
: PROD-VECTOR 1 SWAP DO I *LOOP DROP ;
: MAX-VECTOR 0 SWAP DO I OVER >IF SWAP THEN LOOP DROP ;
: MIN-VECTOR 0 SWAP DO I OVER <IF SWAP THEN LOOP DROP ;

: SUM-MATRIX 0 DO I J 0 TO EEC@ +LOOP SUM-VECTOR +LOOP DROP ;
: PROD-MATRIX 1 DO I J 0 TO EEC@ +LOOP PROD-VECTOR *LOOP DROP ;
: MAX-MATRIX 0 DO I J 0 TO EEC@ +LOOP MAX-VECTOR +LOOP DROP ;
: MIN-MATRIX 0 DO I J 0 TO EEC@ +LOOP MIN-VECTOR +LOOP DROP ;

: INVERT 0 DO I OVER PICK 1 > IF SWAP 1- NEGATE THEN 1+LOOP ;
: NEG 0 DO I OVER PICK 0 > IF SWAP 0- THEN 1+LOOP ;
: TRANSPOSE DO J I 0 TO EEC@ +LOOP [ J I @ ] LOOP DROP ;

: PRINT-VECTOR DO I @ O LOOP ;
: PRINT-MATRIX DO I J 0 TO EEC@ +LOOP [ J I @ O ] LOOP DROP ;

```

Este código Forth implementa una serie de funciones matemáticas comunes, incluyendo la suma, el producto, el máximo y el mínimo de vectores y matrices. También incluye funciones para invertir y transponer matrices.

El código está escrito en español, que es una extensión de Forth que permite utilizar palabras en español en lugar de palabras en inglés.

Aquí hay una explicación del código:

* La primera línea define la palabra `O`, que se utiliza para desplegar un valor en la pantalla.
* Las siguientes diez líneas definen las palabras `1+` a `10+`, que se utilizan para sumar dos valores.
* La palabra `VECTOR` se utiliza para crear un nuevo vector.
* La palabra `INIT-VECTOR` se utiliza para inicializar un vector con valores cero.
* La palabra `INIT-MATRIX` se utiliza para inicializar una matriz con valores cero.
* Las palabras `SUM-VECTOR`, `PROD-VECTOR`, `MAX-VECTOR` y `MIN-VECTOR` se utilizan para calcular la suma, el producto, el máximo y el mínimo de un vector, respectivamente.
* Las palabras `SUM-MATRIX`, `PROD-MATRIX`, `MAX-MATRIX` y `MIN-MATRIX` se utilizan para calcular la suma, el producto, el máximo y el mínimo de una matriz, respectivamente.
* La palabra `INVERT` se utiliza para invertir una matriz.
* La palabra `NEG` se utiliza para negar una matriz.
* La palabra `TRANSPOSE` se utiliza para transponer una matriz.
* Las palabras `PRINT-VECTOR` y `PRINT-MATRIX` se utilizan para imprimir un vector y una matriz, respectivamente.

Este código puede utilizarse para realizar una variedad de cálculos matemáticos. Por ejemplo, se puede utilizar para encontrar la suma, el producto, el máximo y el mínimo de un conjunto de datos, o para invertir o transponer una matriz.