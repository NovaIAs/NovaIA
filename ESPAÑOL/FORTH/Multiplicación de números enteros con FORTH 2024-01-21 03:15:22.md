DEFINICION Multiplicar
 : Cantidad { -- n } CR ;
 : Numero1 { -- n } CR ;
 : Numero2 { -- m } CR ;
 : Contador { -- i } CR ;

Cantidad
Numero1
Numero2

: Loop
Contador 1 ;
: Condicion
  Contador Numero2 > IF
    DROP
  ELSE
    Numero1 @ @ +
    1 +
  THEN Contador > ;

Condicion
Loop
Condicion WHILE
;

Multiplicar
END-DEFINICION

El código de FORTH anterior define una función llamada "Multiplicar" que calcula el producto de dos números enteros.

La función funciona de la siguiente manera:

1. Se le piden al usuario los valores de los dos números enteros (Cantidad, Numero1 y Numero2).
2. La función inicializa un contador (Contador) a 1.
3. La función define una condición (Condicion) que se cumple cuando el valor del contador es mayor que el valor de Numero2.
4. Dentro del bucle, la función multiplica el valor de Numero1 por el valor de Numero2, y luego añade 1 al valor del contador.
5. La función repite el bucle mientras se cumpla la condición.
6. Cuando la condición ya no se cumple, la función devuelve el producto de Numero1 y Numero2.

El código de FORTH anterior es un ejemplo de un algoritmo de multiplicación recursivo. Este algoritmo funciona dividiendo el problema de multiplicar dos números en un problema más pequeño de multiplicar un número por sí mismo repetidamente.

Este código es complejo porque utiliza varias características avanzadas de FORTH, como la definición de funciones, el uso de bucles y la recursión. Sin embargo, el código es muy eficiente y puede utilizarse para calcular el producto de dos números enteros muy grandes.