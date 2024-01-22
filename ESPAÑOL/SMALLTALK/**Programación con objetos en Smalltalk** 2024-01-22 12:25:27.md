```smalltalk

( 0.5 elevado a 5) mostrar.
```

Este código eleva 0.5 a la quinta potencia y muestra el resultado.

```smalltalk 

"Hola Mundo".
```

Este código simplemente muestra el mensaje "Hola Mundo" en la consola.

```smalltalk 

4 [ :n | 	n mostrar. ] timesRepeat.
```

Este código repite cuatro veces el bloque de código que muestra el valor de `n`. En cada iteración, el valor de `n` es igual al índice actual de la iteración.

```smalltalk 

[1 2 3 4 5 6] select[ :n | n par ].
```

Este código selecciona los elementos pares de la lista `[1 2 3 4 5 6]` utilizando el predicado `n par`. El predicado `n par` devuelve `true` si `n` es par, y `false` en caso contrario.

```smalltalk 

[1 2 3 4 5 6] inyectar: 0 [ :sum n | sum + n ].
```

Este código inyecta el valor 0 en la lista `[1 2 3 4 5 6]` utilizando el bloque de código `:sum n | sum + n`. El bloque de código suma cada elemento de la lista al valor acumulado `sum`. El resultado final es la suma de todos los elementos de la lista.

```smalltalk 

[1 2 3 4 5 6] detectar: [ :n | n == 3 ].
```

Este código detecta el primer elemento de la lista `[1 2 3 4 5 6]` que satisface el predicado `n == 3`. El predicado `n == 3` devuelve `true` si `n` es igual a 3, y `false` en caso contrario.

```smalltalk 

[1 2 3 4 5 6] rechazar: [ :n | n par ].
```

Este código rechaza los elementos pares de la lista `[1 2 3 4 5 6]` utilizando el predicado `n par`. El predicado `n par` devuelve `true` si `n` es par, y `false` en caso contrario.

```smalltalk 

[1 2 3 4 5 6] ordenar.
```

Este código ordena la lista `[1 2 3 4 5 6]` en orden ascendente.

```smalltalk 

"Hola Mundo". longitud.
```

Este código devuelve la longitud de la cadena "Hola Mundo".

```smalltalk 

"Hola Mundo". subCadenaDesde: 6 hasta: 10.
```

Este código devuelve la subcadena de "Hola Mundo" desde el índice 6 hasta el índice 10.

```smalltalk 

"Hola Mundo". caracterEn: 3.
```

Este código devuelve el tercer carácter de la cadena "Hola Mundo".