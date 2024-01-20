```lua
-- Función que calcula el factorial de un número natural.
function factorial(n)
  if n == 0 then
    return 1
  else
    return n * factorial(n-1)
  end
end

-- Función que calcula la suma de los cuadrados de los números naturales hasta un límite.
function suma_cuadrados(n)
  if n == 0 then
    return 0
  else
    return n^2 + suma_cuadrados(n-1)
  end
end

-- Función que calcula el producto de los números naturales hasta un límite.
function producto_naturales(n)
  if n == 0 then
    return 1
  else
    return n * producto_naturales(n-1)
  end
end

-- Función que calcula la potencia de un número a otro.
function potencia(base, exponente)
  if exponente == 0 then
    return 1
  else
    return base * potencia(base, exponente-1)
  end
end

-- Función que calcula el máximo común divisor de dos números naturales.
function mcd(a, b)
  if b == 0 then
    return a
  else
    return mcd(b, a % b)
  end
end

-- Función que calcula el mínimo común múltiplo de dos números naturales.
function mcm(a, b)
  return (a * b) / mcd(a, b)
end

-- Función que calcula la suma de los dígitos de un número natural.
function suma_digitos(n)
  if n == 0 then
    return 0
  else
    return (n % 10) + suma_digitos(n / 10)
  end
end

-- Función que calcula el número de formas de sumar n usando números naturales.
function formas_sumar(n)
  if n == 0 then
    return 1
  else
    local suma = 0
    for i = 1, n do
      suma = suma + formas_sumar(n - i)
    end
    return suma
  end
end
```

Explicación:

* La función `factorial` calcula el factorial de un número natural. El factorial de un número es el producto de todos los números naturales desde ese número hasta 1. Por ejemplo, el factorial de 5 es 5*4*3*2*1 = 120.
* La función `suma_cuadrados` calcula la suma de los cuadrados de los números naturales hasta un límite. Por ejemplo, la suma de los cuadrados de los números naturales hasta 5 es 1^2 + 2^2 + 3^2 + 4^2 + 5^2 = 55.
* La función `producto_naturales` calcula el producto de los números naturales hasta un límite. Por ejemplo, el producto de los números naturales hasta 5 es 1*2*3*4*5 = 120.
* La función `potencia` calcula la potencia de un número a otro. Por ejemplo, la potencia de 2 a 3 es 2^3 = 8.
* La función `mcd` calcula el máximo común divisor de dos números naturales. El máximo común divisor de dos números es el mayor número natural que divide a ambos números. Por ejemplo, el máximo común divisor de 12 y 18 es 6.
* La función `mcm` calcula el mínimo común múltiplo de dos números naturales. El mínimo común múltiplo de dos números es el menor número natural que es divisible por ambos números. Por ejemplo, el mínimo común múltiplo de 12 y 18 es 36.
* La función `suma_digitos` calcula la suma de los dígitos de un número natural. Por ejemplo, la suma de los dígitos de 1234 es 1+2+3+4 = 10.
* La función `formas_sumar` calcula el número de formas de sumar n usando números naturales. Por ejemplo, hay 12 formas de sumar 5: 1 + 1 + 1 + 1 + 1, 1 + 1 + 1 + 2, 1 + 1 + 3, 1 + 2 + 2, 1 + 4, 2 + 1 + 1 + 1, 2 + 1 + 2, 2 + 3, 3 + 1 + 1, 3 + 2, 4 + 1, 5.