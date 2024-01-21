```lua
-- Definición de funciones

-- Función para calcular el factorial de un número
function factorial(n)
  if (n == 0) then
    return 1
  else
    return n * factorial(n-1)
  end
end

-- Función para calcular la combinación de dos números (n y k)
function combinacion(n, k)
  if (k == 0 or k == n) then
    return 1
  else
    return combinacion(n-1, k-1) + combinacion(n-1, k)
  end
end

-- Función para calcular la permutación de dos números (n y k)
function permutacion(n, k)
  if (k == 0) then
    return 1
  else
    return n * permutacion(n-1, k-1)
  end
end

-- Función para calcular el máximo común divisor de dos números (a y b)
function mcd(a, b)
  if (b == 0) then
    return a
  else
    return mcd(b, a % b)
  end
end

-- Función para calcular el mínimo común múltiplo de dos números (a y b)
function mcm(a, b)
  return (a * b) / mcd(a, b)
end

-- Función para calcular el coeficiente binomial de dos números (n y k)
function coeficiente_binomial(n, k)
  return factorial(n) / (factorial(k) * factorial(n-k))
end

-- Función para calcular la distancia euclidiana entre dos puntos (x1, y1) y (x2, y2)
function distancia_euclidiana(x1, y1, x2, y2)
  return math.sqrt((x2 - x1)^2 + (y2 - y1)^2)
end

-- Función para calcular el área de un triángulo a partir de sus tres lados (a, b y c)
function area_triangulo(a, b, c)
  s = (a + b + c) / 2
  return math.sqrt(s * (s - a) * (s - b) * (s - c))
end

-- Función para calcular el volumen de una esfera a partir de su radio (r)
function volumen_esfera(r)
  return (4 / 3) * math.pi * r^3
end

-- Función para calcular la superficie de una esfera a partir de su radio (r)
function superficie_esfera(r)
  return 4 * math.pi * r^2
end

-- Función para calcular la derivada de una función f(x) en un punto x
function derivada(f, x)
  h = 0.0001
  return (f(x + h) - f(x)) / h
end

-- Función para calcular la integral definida de una función f(x) entre los límites a y b
function integral(f, a, b)
  n = 1000
  h = (b - a) / n
  suma = 0
  for i = 1 to n do
    suma = suma + f(a + (i - 0.5) * h)
  end
  return h * suma
end

-- Ejemplo de uso

-- Calcular el factorial de 5
print(factorial(5))

-- Calcular la combinación de 10 y 5
print(combinacion(10, 5))

-- Calcular la permutación de 10 y 5
print(permutacion(10, 5))

-- Calcular el máximo común divisor de 24 y 18
print(mcd(24, 18))

-- Calcular el mínimo común múltiplo de 24 y 18
print(mcm(24, 18))

-- Calcular el coeficiente binomial de 10 y 5
print(coeficiente_binomial(10, 5))

-- Calcular la distancia euclidiana entre los puntos (1, 2) y (3, 4)
print(distancia_euclidiana(1, 2, 3, 4))

-- Calcular el área de un triángulo con lados 3, 4 y 5
print(area_triangulo(3, 4, 5))

-- Calcular el volumen de una esfera con radio 5
print(volumen_esfera(5))

-- Calcular la superficie de una esfera con radio 5
print(superficie_esfera(5))

-- Calcular la derivada de la función f(x) = x^2 en el punto 2
print(derivada(function(x) return x^2 end, 2))

-- Calcular la integral definida de la función f(x) = x^2 entre los límites 0 y 1
print(integral(function(x) return x^2 end, 0, 1))
```

Explicación:

* El código anterior define una serie de funciones matemáticas comunes en Lua, incluyendo cálculo factorial, combinación, permutación, máximo común divisor, mínimo común múltiplo, coeficiente binomial, distancia euclidiana, área del triángulo, volumen de la esfera, superficie de la esfera, derivada e integral definida.
* Cada función toma los parámetros necesarios y devuelve el resultado calculado.
* El código también incluye un ejemplo de uso que muestra cómo usar cada función.
* Este código es un ejemplo de programación funcional en Lua y puede ser útil para realizar cálculos matemáticos complejos.