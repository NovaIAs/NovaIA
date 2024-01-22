```forth
: esPrimo ( n -- flag )
    2 >dup
    swap dup
    sqrt >r>
    begin
        over swap mod 0= until
    drop ;

: suma-primos ( n -- suma )
    1 100 do
        dup esPrimo if over + then
    loop
    drop ;

: escribi-primos ( n -- )
    1 n do
        dup esPrimo if . then
    loop ;
```

Este código Forth calcula la suma de los números primos entre 1 y 100 y luego los muestra en la pantalla.

El código comienza definiendo una palabra llamada `esPrimo` que toma un número como argumento y devuelve un indicador que es verdadero si el número es primo y falso si no lo es. La palabra `esPrimo` funciona probando si el número es divisible por cualquier número entre 2 y la raíz cuadrada del número. Si el número es divisible por alguno de estos números, entonces no es primo y la palabra `esPrimo` devuelve falso. De lo contrario, la palabra `esPrimo` devuelve verdadero.

El siguiente paso es definir una palabra llamada `suma-primos` que toma un número como argumento y devuelve la suma de los números primos entre 1 y el número dado. La palabra `suma-primos` utiliza la palabra `esPrimo` para determinar qué números son primos y luego suma estos números.

El último paso es definir una palabra llamada `escribi-primos` que toma un número como argumento y muestra en la pantalla los números primos entre 1 y el número dado. La palabra `escribi-primos` utiliza la palabra `esPrimo` para determinar qué números son primos y luego muestra estos números en la pantalla.

Para usar el código, primero debe cargar el intérprete Forth en su computadora. Luego, puede escribir las siguientes líneas de código en el intérprete Forth:

```forth
esPrimo 17 esPrimo 18

suma-primos 100

escribi-primos 100
```

Cuando ejecute estas líneas de código, el intérprete Forth calculará la suma de los números primos entre 1 y 100 y luego mostrará los números primos entre 1 y 100 en la pantalla.