```modula-2
MODULE Potencias;
FROM FormatString IMPORT FormatString;

PROCEDURE ModExp(base: LONGINT; exp: LONGINT; m: LONGINT): LONGINT;
VAR
    res: LONGINT;
    tmp: LONGINT;
BEGIN
    res := 1;
    IF exp = 0 THEN
        RETURN 1
    END;
    tmp := ModExp(base, exp DIV 2, m);
    res := (tmp * tmp) MOD m;
    IF exp MOD 2 = 1 THEN
        res := (res * base) MOD m
    END;
    RETURN res
END ModExp;

PROCEDURE Test();
VAR
    base: LONGINT;
    exp: LONGINT;
    m: LONGINT;
    res: LONGINT;
BEGIN
    base := 2;
    exp := 10;
    m := 1000;
    res := ModExp(base, exp, m);
    FormatString("Resultado: %d\n", [res])
END Test;

BEGIN
    Test()
END Potencias.
```

Explicación del código:

* El módulo `Potencias` define una función llamada `ModExp` que calcula la potencia de un número dado utilizando el algoritmo de modularización.
* La función `ModExp` recibe tres parámetros: `base`, que es el número al que se le va a elevar a la potencia; `exp`, que es el exponente; y `m`, que es el módulo.
* La función `ModExp` comienza inicializando una variable `res` al valor 1.
* Si `exp` es igual a 0, la función devuelve el valor 1 y termina.
* La función calcula la potencia de `base` elevada a la mitad de `exp` utilizando la función `ModExp`.
* La función multiplica el resultado anterior por sí mismo y lo guarda en la variable `res`.
* Si `exp` es impar, la función multiplica `res` por `base` y lo guarda en `res`.
* La función devuelve el valor de `res`.
* La función `Test` crea una prueba para la función `ModExp`.
* La función `Test` inicializa las variables `base`, `exp` y `m` con los valores 2, 10 y 1000, respectivamente.
* La función `Test` llama a la función `ModExp` con los valores de `base`, `exp` y `m` y guarda el resultado en la variable `res`.
* La función `Test` imprime el valor de `res` en la consola utilizando la función `FormatString`.
* La función `Test` termina.
* El módulo `Potencias` termina.