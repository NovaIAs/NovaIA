```lua
-- Función que calcula el factorial de un número entero positivo
function factorial(n)
  if n == 0 then
    return 1  -- Caso base: factorial de 0 es 1
  else
    return n * factorial(n-1)  -- Caso recursivo: factorial de n es n * factorial(n-1)
  end
end

-- Función que calcula la serie de Fibonacci hasta un número dado de términos
function fibonacci(n)
  if n <= 1 then
    return n  -- Casos base: Fibonacci de 0 y 1 es 0 y 1, respectivamente
  else
    return fibonacci(n-1) + fibonacci(n-2)  -- Caso recursivo: Fibonacci de n es Fibonacci de n-1 más Fibonacci de n-2
  end
end

-- Función que calcula el máximo común divisor de dos números enteros positivos
function mcd(a, b)
  if b == 0 then
    return a  -- Caso base: mcd de un número y 0 es el propio número
  else
    return mcd(b, a % b)  -- Caso recursivo: mcd de a y b es mcd de b y el resto de a entre b
  end
end

-- Función que calcula el mínimo común múltiplo de dos números enteros positivos
function mcm(a, b)
  return a * b / mcd(a, b)  -- mcm de a y b es el producto de a y b dividido por su mcd
end

-- Función que calcula la raíz cuadrada de un número real positivo
function sqrt(n)
  if n < 0 then
    return nil  -- Caso base: no hay raíz cuadrada real para números negativos
  else
    local guess = n / 2  -- Adivinar la raíz cuadrada inicial es la mitad del número
    local epsilon = 0.0001  -- Precisión deseada
    while math.abs(guess * guess - n) > epsilon do  -- Iterar hasta que la aproximación sea lo suficientemente precisa
      guess = (guess + n / guess) / 2  -- Aproximación de la raíz cuadrada mediante el método de Newton-Raphson
    end
    return guess  -- Devolver la aproximación final
  end
end

-- Función que calcula el área de un círculo dado su radio
function area_circulo(r)
  return math.pi * r * r  -- Área de un círculo es pi por radio al cuadrado
end

-- Función que calcula el volumen de una esfera dado su radio
function volumen_esfera(r)
  return (4/3) * math.pi * r * r * r  -- Volumen de una esfera es (4/3) por pi por radio al cubo
end

-- Función que calcula el área de un triángulo dado su base y su altura
function area_triangulo(b, h)
  return (1/2) * b * h  -- Área de un triángulo es la mitad de la base por la altura
end

-- Función que calcula el volumen de un cilindro dado su radio y su altura
function volumen_cilindro(r, h)
  return math.pi * r * r * h  -- Volumen de un cilindro es pi por radio al cuadrado por altura
end
```

Explicación del código:

* La función `factorial` calcula el factorial de un número entero positivo utilizando recursión.
* La función `fibonacci` calcula la serie de Fibonacci hasta un número dado de términos utilizando recursión.
* La función `mcd` calcula el máximo común divisor de dos números enteros positivos utilizando el algoritmo de Euclides.
* La función `mcm` calcula el mínimo común múltiplo de dos números enteros positivos utilizando el mcd.
* La función `sqrt` calcula la raíz cuadrada de un número real positivo utilizando el método de Newton-Raphson.
* La función `area_circulo` calcula el área de un círculo dado su radio.
* La función `volumen_esfera` calcula el volumen de una esfera dado su radio.
* La función `area_triangulo` calcula el área de un triángulo dado su base y su altura.
* La función `volumen_cilindro` calcula el volumen de un cilindro dado su radio y su altura.