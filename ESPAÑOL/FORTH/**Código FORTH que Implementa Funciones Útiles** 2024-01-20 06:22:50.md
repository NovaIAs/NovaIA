**Código FORTH**

```
: SALUDO ( -- )
    "¡Hola, mundo!" CR
;

: SUMA ( a b -- c )
    a b +
;

: RESTA ( a b -- c )
    a b -
;

: MULTIPLICACIÓN ( a b -- c )
    a b *
;

: DIVISIÓN ( a b -- c )
    a b /
;

: POTENCIA ( a b -- c )
    a b ^
;

: RAÍZ_CUADRADA ( a -- b )
    a sqrt
;

: FACTORIAL ( n -- f )
    0 n ?DO [ i dup * ] LOOP DROP
;

: FIBONACCI ( n -- f )
    0 1 BEGIN
        dup 2 > WHILE [ swap over + ] REPEAT DROP
;

: PRIMOS ( n -- p )
    1 2 BEGIN
        [ 2 > WHILE [ i dup mod 0 = IF DROP LEAVE ENDIF ] 2 * + ]
    WHILE DROP
;

: PALÍNDROMO? ( s -- b )
    0 r> BEGIN [ s @ dup 0 = WHILE [ DROP LEAVE ] REPEAT 2SWAP ] WHILE
;

: ORDENAR ( l -- l' )
    [ BEGIN [ i j @ > WHILE [ j swap @ i swap ! j + ] REPEAT ] 0 + ]
    WHILE DROP
;

: BUSCAR ( l e -- i )
    [ BEGIN [ i l @ e = WHILE [ LEAVE ] REPEAT i 1 + ] 0 + ]
    WHILE DROP
;

: IMPRIMIR ( l -- )
    [ L @ TYPE ] WHILE DROP
;
```

**Explicación del código**

El código anterior implementa varias funciones útiles en FORTH, incluyendo:

* **SALUDO:** Imprime el mensaje "¡Hola, mundo!" en la pantalla.
* **SUMA:** Suma dos números.
* **RESTA:** Resta un número de otro.
* **MULTIPLICACIÓN:** Multiplica dos números.
* **DIVISIÓN:** Divide un número por otro.
* **POTENCIA:** Eleva un número a una potencia.
* **RAÍZ_CUADRADA:** Calcula la raíz cuadrada de un número.
* **FACTORIAL:** Calcula el factorial de un número.
* **FIBONACCI:** Calcula el n-ésimo número de Fibonacci.
* **PRIMOS:** Genera una lista de números primos hasta un número dado.
* **PALÍNDROMO?:** Comprueba si una palabra es un palíndromo.
* **ORDENAR:** Ordena una lista de números.
* **BUSCAR:** Busca un elemento en una lista.
* **IMPRIMIR:** Imprime una lista de elementos en la pantalla.

Estas funciones se pueden utilizar para realizar una variedad de tareas, incluyendo cálculos matemáticos, procesamiento de texto y generación de listas.