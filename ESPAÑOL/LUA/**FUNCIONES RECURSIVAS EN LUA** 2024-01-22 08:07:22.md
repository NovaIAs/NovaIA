```lua
-- Definición de la función recursiva para calcular el factorial de un número.

function factorial(n)
    if n == 0 then
        return 1  -- Caso base de la recursividad: el factorial de 0 es 1.
    else
        return n * factorial(n-1)  -- Caso recursivo: el factorial de n es n multiplicado por el factorial de n-1.
    end
end

-- Definición de la función para calcular la suma de los dígitos de un número.

function sumaDigitos(n)
    if n < 10 then
        return n  -- Caso base de la recursividad: si el número es menor que 10, se devuelve él mismo.
    else
        return (n % 10) + sumaDigitos(n / 10)  -- Caso recursivo: se suma el último dígito del número con la suma de los dígitos del número sin el último dígito.
    end
end

-- Definición de la función para calcular el producto de los elementos de un array.

function productoArray(arr)
    if #arr == 0 then
        return 1  -- Caso base de la recursividad: si el array está vacío, se devuelve 1.
    else
        return arr[1] * productoArray(arr:sub(2))  -- Caso recursivo: se multiplica el primer elemento del array por el producto de los elementos restantes del array.
    end
end

-- Definición de la función para comprobar si un número es primo.

function esPrimo(n)
    if n < 2 then
        return false  -- Los números menores que 2 no son primos.
    elseif n == 2 then
        return true  -- El número 2 es primo.
    elseif n % 2 == 0 then
        return false  -- Los números pares mayores que 2 no son primos.
    else
        local divisor = 3
        while divisor <= math.sqrt(n) do
            if n % divisor == 0 then
                return false  -- Si el número es divisible por alguno de sus divisores, no es primo.
            end
            divisor = divisor + 2  -- Se incrementa el divisor de dos en dos para comprobar la divisibilidad por números impares.
        end
        return true  -- Si el número no es divisible por ninguno de sus divisores, es primo.
    end
end

-- Definición de la función para generar una lista de números primos hasta un límite dado.

function primosHasta(n)
    local primos = {}  -- Se crea una lista vacía para almacenar los números primos.
    for i = 2, n do
        if esPrimo(i) then
            primos:append(i)  -- Si el número es primo, se añade a la lista.
        end
    end
    return primos  -- Se devuelve la lista de números primos.
end

-- Definición de la función para calcular el máximo común divisor de dos números.

function maximoComunDivisor(a, b)
    if b == 0 then
        return a  -- Si el segundo número es 0, el máximo común divisor es el primer número.
    else
        return maximoComunDivisor(b, a % b)  -- Caso recursivo: se calcula el máximo común divisor del segundo número y el resto de la división del primer número entre el segundo.
    end
end

-- Definición de la función para calcular el mínimo común múltiplo de dos números.

function minimoComunMultiplo(a, b)
    return math.abs(a * b) / maximoComunDivisor(a, b)  -- Se calcula el mínimo común múltiplo como el producto de los dos números dividido por el máximo común divisor.
end
```

Explicación del código:

* La función `factorial` calcula el factorial de un número utilizando recursividad.
* La función `sumaDigitos` calcula la suma de los dígitos de un número utilizando recursividad.
* La función `productoArray` calcula el producto de los elementos de un array utilizando recursividad.
* La función `esPrimo` comprueba si un número es primo.
* La función `primosHasta` genera una lista de números primos hasta un límite dado.
* La función `maximoComunDivisor` calcula el máximo común divisor de dos números.
* La función `minimoComunMultiplo` calcula el mínimo común múltiplo de dos números.