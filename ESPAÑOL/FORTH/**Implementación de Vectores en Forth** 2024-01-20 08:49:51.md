```forth

: 10 20 30 40 50 60 70 80 90 100 words (Vector 1)

: MULTIPLICAR , dup @ * drop ;

: VECSUM 0 swap do
    @ over MULTIPLICAR + loop drop ;

: MAKE-VECTOR , do i swap 0 @ i store +loop ;

100 1 MAKE-VECTOR (Vector 2)

: VECFILL 0 swap do i over @ swap i +! loop ;

: VECSWAP over do dup @ @ swap ! loop ;

: VECPRINT 0 swap do @ . loop ;

10 20 30 40 50 60 70 80 90 100 VECFILL (Vector 1)

100 VECSWAP

100 VECPRINT (Vector 1)

CrLf

100 VECPRINT (Vector 2)

CrLf

100 VECSUM (Suma del vector 1)

```

Explicación del código:

* La primera línea crea un vector de 100 palabras con los valores del 10 al 100.
* La segunda línea define una palabra llamada `MULTIPLICAR` que multiplica dos números.
* La tercera línea define una palabra llamada `VECSUM` que calcula la suma de los elementos de un vector.
* La cuarta línea define una palabra llamada `MAKE-VECTOR` que crea un vector de un tamaño especificado.
* La quinta línea crea un vector de 100 palabras con el valor 0.
* La sexta línea define una palabra llamada `VECFILL` que rellena un vector con un valor especificado.
* La séptima línea define una palabra llamada `VECSWAP` que intercambia los valores de dos vectores.
* La octava línea define una palabra llamada `VECPRINT` que imprime los valores de un vector.
* La novena línea rellena el vector 1 con los valores del 10 al 100.
* La décima línea intercambia los valores de los vectores 1 y 2.
* La undécima línea imprime los valores del vector 1.
* La duodécima línea imprime los valores del vector 2.
* La decimotercera línea calcula la suma de los elementos del vector 1 y la imprime.