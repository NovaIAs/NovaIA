```forth
: constant-array ( n a -- )
  ( n -- ) dup >r
  [ ( r: a -- ) r> r> +loop ]
  r> drop ;

: principal ( n -- )
dup constant-array 0r
do
dup i @ 2roll + swap !
i 1+ loop
drop ;

\$ main
dup 10 principal 0= if
  "ERROR: n debe ser mayor o igual que 1\n" crlf
  quit
then

dup 2>r 0r
do
dup r: i 2cells + swap @ c.r
r> 1+ loop

r> drop
\$
```

Explicación:

* La palabra `constant-array` crea una matriz de enteros de tamaño `n` y la llena con el valor `a`.
* La palabra `principal` recibe un número `n` y crea una matriz de enteros de tamaño `n`, la llena con los números del 1 al `n`, y luego imprime la matriz.
* La palabra `main` llama a la palabra `principal` con el valor de `n` ingresado por el usuario.
* La palabra `\$` es una palabra de Forth que se usa para definir funciones.
* La palabra `dup` duplica el elemento superior de la pila.
* La palabra `10` empuja el número 10 a la pila.
* La palabra `principal` recibe un argumento de la pila y lo empuja a la pila.
* La palabra `0=` compara el argumento superior de la pila con el valor 0.
* La palabra `if` ejecuta el código dentro de su cuerpo si el argumento superior de la pila es verdadero.
* La palabra `then` termina el cuerpo de un condicional.
* La palabra `quit` sale de Forth.
* La palabra `2>r` empuja el registro `r` a la pila y lo configura para que apunte al segundo argumento de la pila.
* La palabra `0r` configura el registro `r` para que apunte al tercer argumento de la pila.
* La palabra `do` ejecuta el código dentro de su cuerpo `n` veces, donde `n` es el argumento superior de la pila.
* La palabra `dup` duplica el elemento superior de la pila.
* La palabra `r: i 2cells + swap @` obtiene el valor del registro `r` más el desplazamiento `i` y lo empuja a la pila.
* La palabra `c.r` imprime el carácter superior de la pila como un carácter ASCII.
* La palabra `r> 1+` suma 1 al registro `r`.
* La palabra `loop` ejecuta el código dentro de su cuerpo hasta que el argumento superior de la pila sea cero.
* La palabra `r> drop` libera el argumento superior de la pila y configura el registro `r` para que apunte al siguiente argumento de la pila.
* La palabra `\$` es una palabra de Forth que se usa para definir funciones.