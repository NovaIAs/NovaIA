```
-- Programa en COOL para calcular el máximo común divisor y el mínimo común múltiplo de dos números.

-- Define la función para calcular el máximo común divisor.
máximoComúnDivisor: function (a: Int, b: Int): Int is
    if b = 0 then
        return a;
    end if;
    return máximoComúnDivisor(b, a mod b);
end máximoComúnDivisor;

-- Define la función para calcular el mínimo común múltiplo.
mínimoComúnMúltiplo: function (a: Int, b: Int): Int is
    return (a * b) div máximoComúnDivisor(a, b);
end mínimoComúnMúltiplo;

-- Lee los dos números del usuario.
print("Introduce el primer número:");
primerNúmero: Int := readInt();
print("Introduce el segundo número:");
segundoNúmero: Int := readInt();

-- Calcula el máximo común divisor y el mínimo común múltiplo.
mcd: Int := máximoComúnDivisor(primerNúmero, segundoNúmero);
mcm: Int := mínimoComúnMúltiplo(primerNúmero, segundoNúmero);

-- Muestra el máximo común divisor y el mínimo común múltiplo.
print("El máximo común divisor de ", primerNúmero, " y ", segundoNúmero, " es ", mcd);
print("El mínimo común múltiplo de ", primerNúmero, " y ", segundoNúmero, " es ", mcm);
```

Explicación:

* El programa primero define dos funciones: `máximoComúnDivisor` y `mínimoComúnMúltiplo`.
* La función `máximoComúnDivisor` calcula el máximo común divisor de dos números usando el algoritmo de Euclides.
* La función `mínimoComúnMúltiplo` calcula el mínimo común múltiplo de dos números usando la fórmula `mcm = (a * b) div mcd`, donde `a` y `b` son los dos números y `mcd` es el máximo común divisor de `a` y `b`.
* El programa luego lee los dos números del usuario.
* A continuación, calcula el máximo común divisor y el mínimo común múltiplo de los dos números llamando a las funciones `máximoComúnDivisor` y `mínimoComúnMúltiplo`.
* Finalmente, el programa muestra el máximo común divisor y el mínimo común múltiplo en la consola.