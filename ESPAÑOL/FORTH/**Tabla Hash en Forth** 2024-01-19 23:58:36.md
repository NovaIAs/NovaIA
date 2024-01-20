```forth
: HASH-TABLA ( -- )
  128 100 CELLS ALLOT
  0 0 , , 1 DO
    , LOOP ;

: HASH-INS ( KEY VAL -- )
  HASH-TABLA OVER + [C@] @ XOR 4 + SWAP ! ;

: HASH-GET ( KEY -- VAL )
  HASH-TABLA OVER + [C@] @ XOR 4 + SWAP @ ;

: HASH-DEL ( KEY -- )
  HASH-TABLA OVER + [C@] @ XOR 4 + 0 ! ;

: DEMO ( -- )
  HASH-TABLA
  "HOLA" 100 HASH-INS
  "MUNDO" 200 HASH-INS
  "!" 300 HASH-INS
  "HOLA" HASH-GET PRINT
  "MUNDO" HASH-GET PRINT
  "!" HASH-GET PRINT
  "NO-EXISTE" HASH-GET PRINT
  "HOLA" HASH-DEL
  "HOLA" HASH-GET PRINT ;

DEMO
```

Explicación:

* La primera línea crea una tabla hash con 128 entradas. Cada entrada es una celda de 4 bytes.
* La segunda línea inicializa la tabla hash con 0s.
* La función `HASH-INS` inserta un par clave-valor en la tabla hash. La clave se convierte en un índice de la tabla hash usando la función `XOR`. El valor se almacena en la entrada correspondiente de la tabla hash.
* La función `HASH-GET` recupera el valor asociado a una clave de la tabla hash. Utiliza la misma función `XOR` para convertir la clave en un índice de la tabla hash y luego recupera el valor de la entrada correspondiente.
* La función `HASH-DEL` elimina un par clave-valor de la tabla hash. Utiliza la misma función `XOR` para convertir la clave en un índice de la tabla hash y luego establece el valor de la entrada correspondiente en 0.
* La función `DEMO` crea la tabla hash e inserta los pares clave-valor `"HOLA"`, `"MUNDO"` y `"!"`. A continuación, recupera los valores asociados a cada clave e imprime el resultado. También intenta recuperar el valor asociado a la clave `"NO-EXISTE"`, que no existe en la tabla hash, y se imprime un mensaje de error. Por último, elimina el par clave-valor `"HOLA"` de la tabla hash y vuelve a recuperar el valor asociado a `"HOLA"`, que ahora es 0.