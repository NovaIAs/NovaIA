```forth

: FIB
    0 1
    WHILE
        DUP 2+ SWAP OVER + SWAP OVER
    REPEAT
    DROP ;

: PRIMOS
    \ paridad
    SWAP 2 MOD
    0= IF
        DROP
    ELSE
        2 2DUP = IF THEN
        2+ 2DUP 1+ OVER I
        WHILE
            SWAP DUP MOD
            0= IF DROP EXIT THEN
        REPEAT
    THEN ;

: FACT
    0 SWAP UNTIL
    1 LOOP ;

: COMB
    >R 0 SWAP DO
        OVER OVER - 1 - MAX
    LOOP 2DROP R> ;

: POT
    DUP SWAP 0 DO R> R> I LOOP 2DROP ;

: SUM
    BEGIN
        SWAP DUP 0 DO
            SWAP I +
        LOOP
        DROP
    AGAIN ;

: MULT
    BEGIN
        SWAP DUP 0 DO
            SWAP I *
        LOOP
        DROP
    AGAIN ;

: RESULT
    ( RESOLUCIÓN ) . CR ;


\ FIBONACCI

DUP FIB
RESULT

\ PRIMOS

100 PRIMOS
RESULT

\ FACTORIAL

5 FACT
RESULT

\ COMBINACIONES

6 4 COMB
RESULT

\ POTENCIA

2 7 POT
RESULT

\ SUMA

10 100 SUM
RESULT

\ MULTIPLICACIÓN

10 100 MULT
RESULT

```

Este código Forth implementa las siguientes funciones:

* **FIB:** Calcula la serie de Fibonacci hasta un número determinado.
* **PRIMOS:** Encuentra todos los números primos hasta un número determinado.
* **FACT:** Calcula el factorial de un número.
* **COMB:** Calcula el número de combinaciones de un conjunto de elementos.
* **POT:** Calcula la potencia de un número.
* **SUM:** Calcula la suma de dos números.
* **MULT:** Calcula el producto de dos números.

El código utiliza las siguientes palabras Forth:

* **0:** Empuja el número 0 en la pila.
* **1:** Empuja el número 1 en la pila.
* **2:** Empuja el número 2 en la pila.
* **2+:** Suma 2 al número en la parte superior de la pila.
* **2DUP:** Duplica el número en la parte superior de la pila dos veces.
* **SWAP:** Intercambia los dos números en la parte superior de la pila.
* **OVER:** Copia el número en la parte superior de la pila en la segunda posición.
* **+:** Suma los dos números en la parte superior de la pila.
* **-:** Resta los dos números en la parte superior de la pila.
* **MAX:** Devuelve el número más grande de los dos números en la parte superior de la pila.
* **DO:** Repite una serie de instrucciones un número determinado de veces.
* **LOOP:** Termina una instrucción DO.
* **DROP:** Elimina el número en la parte superior de la pila.
* **BEGIN:** Inicia un bloque de código.
* **AGAIN:** Repite un bloque de código.
* **=: IF:** Comprueba si los dos números en la parte superior de la pila son iguales.
* **THEN:** Ejecuta un bloque de código si la condición es verdadera.
* **ELSE:** Ejecuta un bloque de código si la condición es falsa.
* **EXIT:** Sale de una instrucción DO o WHILE.
* **WHILE:** Repite una serie de instrucciones mientras una condición sea verdadera.
* **UNTIL:** Repite una serie de instrucciones hasta que una condición sea verdadera.
* **R>:** Rota los tres números en la parte superior de la pila hacia la derecha.
* **R>:** Rota los dos números en la parte superior de la pila hacia la derecha.
* **I:** Incrementa el número en la parte superior de la pila en 1.
* **( ):** Empuja una cadena de texto en la pila.
* **. CR:** Imprime una cadena de texto en la consola y realiza un salto de línea.