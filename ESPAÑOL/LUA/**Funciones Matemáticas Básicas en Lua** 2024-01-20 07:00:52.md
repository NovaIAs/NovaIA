```lua
-- Esta función calcula el factorial de un número.
function factorial(n)
  if n == 0 then
    return 1
  else
    return n * factorial(n-1)
  end
end

-- Esta función calcula la suma de los dígitos de un número.
function sumDigits(n)
  if n < 10 then
    return n
  else
    return (n % 10) + sumDigits(n // 10)
  end
end

-- Esta función calcula el producto de los dígitos de un número.
function productDigits(n)
  if n < 10 then
    return n
  else
    return (n % 10) * productDigits(n // 10)
  end
end

-- Esta función calcula el mayor común divisor de dos números.
function gcd(a, b)
  if b == 0 then
    return a
  else
    return gcd(b, a % b)
  end
end

-- Esta función calcula el mínimo común múltiplo de dos números.
function lcm(a, b)
  return (a * b) / gcd(a, b)
end

-- Esta función calcula la distancia entre dos puntos en un plano cartesiano.
function distance(x1, y1, x2, y2)
  return math.sqrt((x2 - x1)^2 + (y2 - y1)^2)
end

-- Esta función calcula el área de un triángulo.
function areaTriangle(base, height)
  return (base * height) / 2
end

-- Esta función calcula el área de un círculo.
function areaCircle(radius)
  return math.pi * radius^2
end

-- Esta función calcula el volumen de un cubo.
function volumeCube(side)
  return side^3
end

-- Esta función calcula el volumen de una esfera.
function volumeSphere(radius)
  return (4/3) * math.pi * radius^3
end
```

Explicación:

* La función `factorial` calcula el factorial de un número usando recursión.
* La función `sumDigits` calcula la suma de los dígitos de un número usando recursión.
* La función `productDigits` calcula el producto de los dígitos de un número usando recursión.
* La función `gcd` calcula el mayor común divisor de dos números usando el algoritmo de Euclides.
* La función `lcm` calcula el mínimo común múltiplo de dos números usando el algoritmo de Euclides.
* La función `distance` calcula la distancia entre dos puntos en un plano cartesiano usando el teorema de Pitágoras.
* La función `areaTriangle` calcula el área de un triángulo usando la fórmula (base * altura) / 2.
* La función `areaCircle` calcula el área de un círculo usando la fórmula \(\pi r^2\).
* La función `volumeCube` calcula el volumen de un cubo usando la fórmula \(s^3\).
* La función `volumeSphere` calcula el volumen de una esfera usando la fórmula \((4/3) \pi r^3\).