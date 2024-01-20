```forth

: NUMERO-ROMANO
       2DUP SWAP > IF
             2DUP SWAP 1+ ."M"
             2DUP SWAP < IF
                     4DUP SWAP C@ 90 = IF ."CM" EXIT THEN
                     4DUP SWAP C@ 50 = IF ."D" EXIT THEN
                     2DUP SWAP1+ ."C"
             THEN
             2DUP SWAP < IF
                     4DUP SWAP C@ 40 = IF ."XC" EXIT THEN
                     4DUP SWAP C@ 10 = IF ."L" EXIT THEN
                     2DUP SWAP1+ ."X"
             THEN
             2DUP SWAP < IF
                     4DUP SWAP C@ 9 = IF ."IX" EXIT THEN
                     4DUP SWAP C@ 5 = IF ."V" EXIT THEN
                     2DUP SWAP1+ ."I"
             THEN
       ELSE
             ."I"
       THEN ;

: BASE-DEC
      >R 2DUP SWAP 0= IF
             R> DROP EXIT THEN
      10 * R> + R> ;

: ROMANO-DEC
      2DUP SWAP > IF
             2DUP SWAP DUP 1- BASE-DEC
             2DUP SWAP BEGIN
                      2DUP SWAP < WHILE
                             R> + SWAP 1- BASE-DEC
                      REPEAT
                      R> DROP
                      BASE-DEC +
             THEN
      ELSE
             BASE-DEC
      THEN ;


```

Este código toma un número en notación romana y lo convierte a un número decimal.
El código primero verifica si el número romano es mayor que 1000. Si lo es, el código utiliza la palabra "M" para representar 1000. Si el número romano es menor que 1000, el código utiliza la palabra "C" para representar 100. Si el número romano es menor que 100, el código utiliza la palabra "X" para representar 10. Si el número romano es menor que 10, el código utiliza la palabra "I" para representar 1.
El código luego utiliza la palabra "BASE-DEC" para convertir el número romano en un número decimal. La palabra "BASE-DEC" toma un número romano y lo convierte en un número decimal. El código luego utiliza la palabra "ROMANO-DEC" para convertir el número decimal en un número romano. La palabra "ROMANO-DEC" toma un número decimal y lo convierte en un número romano.