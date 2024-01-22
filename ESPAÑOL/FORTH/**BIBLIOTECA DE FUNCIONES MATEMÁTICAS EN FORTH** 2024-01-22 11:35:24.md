```forth
: suma ( n1 -- n2 )
    dup @ swap @ + swap drop ;

: resta ( n1 n2 -- dif )
    dup @ swap @ - swap drop ;

: multiplica ( n1 n2 -- prod )
    dup @ swap @ * swap drop ;

: divide ( n1 n2 -- quot rem )
    dup @ swap @ / swap drop dup ;

: factorial ( n -- n! )
    0 do
        i dup @ 1 + loop
    drop ;

: fibonacci ( n -- n-esimo )
    0 do
        j dup @ swap 1 + i @ j + loop
    drop ;

: potencia ( n1 n2 -- n1^n2 )
    0 1 do
        i dup @ swap dup @ swap multiplica loop
    drop ;

: tabla-de-multiplicar ( n -- )
    0 do
        i dup @ swap 0 do
            j dup @ swap i @ multiplica . space loop
        cr loop
    drop ;

\ Calcula el valor de π mediante la serie de Gregory-Leibniz.
: pi ( n -- serie-de-gregory-leibniz )
    1 1 do
        i dup @ swap 2 * + 1 / loop
    drop ;

\ Calcula el valor de e mediante la serie de Maclaurin.
: e ( n -- serie-de-maclaurin )
    1 1 do
        i dup @ swap 1 + 1 / loop
    drop ;
```

**Explicación:**

Este código Forth implementa una variedad de funciones matemáticas comunes, incluyendo suma, resta, multiplicación, división, factorial, Fibonacci, potencia, tabla de multiplicar, serie de Gregory-Leibniz para π, y serie de Maclaurin para e.

La función `suma` toma dos números del tope de la pila y los suma, dejando el resultado en el tope de la pila.

La función `resta` toma dos números del tope de la pila y los resta, dejando el resultado en el tope de la pila.

La función `multiplica` toma dos números del tope de la pila y los multiplica, dejando el resultado en el tope de la pila.

La función `divide` toma dos números del tope de la pila y los divide, dejando el cociente en el tope de la pila y el resto en el segundo elemento de la pila.

La función `factorial` toma un número del tope de la pila y calcula su factorial, dejando el resultado en el tope de la pila.

La función `fibonacci` toma un número del tope de la pila y calcula el n-ésimo número de Fibonacci, dejando el resultado en el tope de la pila.

La función `potencia` toma dos números del tope de la pila y calcula la potencia del primer número elevado al segundo, dejando el resultado en el tope de la pila.

La función `tabla-de-multiplicar` toma un número del tope de la pila y genera una tabla de multiplicar para ese número, mostrando el resultado de multiplicar ese número por todos los números desde 1 hasta el número dado.

La función `pi` toma un número del tope de la pila y calcula el valor de π mediante la serie de Gregory-Leibniz, dejando el resultado en el tope de la pila.

La función `e` toma un número del tope de la pila y calcula el valor de e mediante la serie de Maclaurin, dejando el resultado en el tope de la pila.