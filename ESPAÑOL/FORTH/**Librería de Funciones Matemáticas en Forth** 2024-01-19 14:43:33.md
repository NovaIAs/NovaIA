```forth
: convertir ( n -- número en base 2 )
    dup 0> while
        swap 10 / mod
        tuck
    repeat drop
;

: decimal ( n -- número en decimales )
    0 begin
        tuck dup mod swap 10 * +
        tuck 10 /
    while drop
;

: factorial( n -- n! )
    0> while dup 1- * repeat drop
;

: fibonacci( n -- n-ésimo número de Fibonacci )
    begin
        dup 1< while
            drop 0
        repeat
        dup 1= while
            drop 1
        repeat
        swap over @ +
    while
;

: suma ( n1 n2 -- n1 + n2 )
    +
;

: resta ( n1 n2 -- n1 - n2 )
    -
;

: multiplicación ( n1 n2 -- n1 * n2 )
    *
;

: división ( n1 n2 -- n1 / n2 )
    /
;

: módulo ( n1 n2 -- n1 mod n2 )
    mod
;

: máximo ( n1 n2 -- n1 máx n2 )
    max
;

: mínimo ( n1 n2 -- n1 min n2 )
    min
;

: raíz cuadrada ( n -- √n )
    sqrt
;

: potencia ( n p -- n^p )
    exp
;

: seno ( n -- sin(n) )
    sin
;

: coseno ( n -- cos(n) )
    cos
;

: tangente ( n -- tan(n) )
    tan
;

: arco seno ( n -- asin(n) )
    asin
;

: arco coseno ( n -- acos(n) )
    acos
;

: arco tangente ( n -- atan(n) )
    atan
;

: exponencial ( n -- e^n )
    exp
;

: logaritmo natural ( n -- ln(n) )
    log
;

: logaritmo en base 10 ( n -- log10(n) )
    log10
;

: absoluto ( n -- |n| )
    abs
;

: signo ( n -- -1 0 1 )
    sign
;

: número primo ( n -- booleano: ¿es n primo? )
    isprime
;

: siguiente primo ( n -- siguiente primo después de n )
    nextprime
;

: factorizar ( n -- lista de factores primos de n )
    factors
;

: mcd ( n1 n2 -- mcd(n1, n2) )
    gcd
;

: mcm ( n1 n2 -- mcm(n1, n2) )
    lcm
;

: número perfecto ( n -- booleano: ¿es n perfecto? )
    isperfect
;

: número deficiente ( n -- booleano: ¿es n deficiente? )
    isdeficient
;

: número abundante ( n -- booleano: ¿es n abundante? )
    isabundant
;

: serie de fibonacci ( n -- lista de los primeros n números de Fibonacci )
    fibseq
;

: secuencia de lucas ( n -- lista de los primeros n números de Lucas )
    lucasseq
;

: triángulo de pascal ( n -- triángulo de Pascal de orden n )
    pascals_triangle
;

: triángulo de sierpinski ( n -- triángulo de Sierpinski de orden n )
    sierpinski_triangle
;

: curvas de hilbert ( n -- curva de Hilbert de orden n )
    hilbert_curve
```

Este código en Forth incluye una amplia gama de funciones matemáticas y de manipulación de números. Aquí hay una explicación de cada función:

1. `convertir`: Esta función convierte un número en su representación en base 2.

2. `decimal`: Esta función convierte un número en su representación en decimales.

3. `factorial`: Esta función calcula el factorial de un número.

4. `fibonacci`: Esta función calcula el n-ésimo número de Fibonacci.

5. `suma`: Esta función suma dos números.

6. `resta`: Esta función resta dos números.

7. `multiplicación`: Esta función multiplica dos números.

8. `división`: Esta función divide dos números.

9. `módulo`: Esta función calcula el módulo de dos números.

10. `máximo`: Esta función devuelve el máximo de dos números.

11. `mínimo`: Esta función devuelve el mínimo de dos números.

12. `raíz cuadrada`: Esta función calcula la raíz cuadrada de un número.

13. `potencia`: Esta función eleva un número a una potencia.

14. `seno`: Esta función calcula el seno de un número.

15. `coseno`: Esta función calcula el coseno de un número.

16. `tangente`: Esta función calcula la tangente de un número.

17. `arco seno`: Esta función calcula el arco seno de un número.

18. `arco coseno`: Esta función calcula el arco coseno de un número.

19. `arco tangente`: Esta función calcula el arco tangente de un número.

20. `exponencial`: Esta función calcula el exponencial de un número.

21. `logaritmo natural`: Esta función calcula el logaritmo natural de un número.

22. `logaritmo en base 10`: Esta función calcula el logaritmo en base 10 de un número.

23. `absoluto`: Esta función calcula el valor absoluto de un número.

24. `signo`: Esta función devuelve el signo de un número.

25. `número primo`: Esta función comprueba si un número es primo.

26. `siguiente primo`: Esta función devuelve el siguiente primo después de un número.

27. `factorizar`: Esta función factoriza un número en sus factores primos.

28. `mcd`: Esta función calcula el máximo común divisor de dos números.

29. `mcm`: Esta función calcula el mínimo común múltiplo de dos números.

30. `número perfecto`: Esta función comprueba si un número es perfecto.

31. `número deficiente`: Esta función comprueba si un número es deficiente.

32. `número abundante`: Esta función comprueba si un número es abundante.

33. `serie de fibonacci`: Esta función genera una lista de los primeros n números de Fibonacci.

34. `secuencia de lucas`: Esta función genera una lista de los primeros n números de Lucas.

35. `triángulo de pascal`: Esta función genera el triángulo de Pascal de orden n.

36. `triángulo de sierpinski`: Esta función genera el triángulo de Sierpinski de orden n.

37. `curvas de hilbert`: Esta función genera la curva de Hilbert de orden n.