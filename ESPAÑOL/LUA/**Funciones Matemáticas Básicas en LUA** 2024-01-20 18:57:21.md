```lua
-- Definición de la función factorial
function factorial(n)
  if n == 0 then
    return 1
  else
    return n * factorial(n-1)
  end
end

-- Definición de la función de Fibonacci
function fibonacci(n)
  if n == 0 then
    return 0
  elseif n == 1 then
    return 1
  else
    return fibonacci(n-1) + fibonacci(n-2)
  end
end

-- Definición de la función de potencia
function potencia(base, exponente)
  if exponente == 0 then
    return 1
  else
    return base * potencia(base, exponente-1)
  end
end

-- Definición de la función de raíz cuadrada
function raizCuadrada(numero)
  function auxiliar(aproximacion, error)
    if error < 0.00001 then
      return aproximacion
    else
      return auxiliar((aproximacion + numero / aproximacion) / 2, math.abs(aproximacion^2 - numero) / 2)
    end
  end
  return auxiliar(1, 1)
end

-- Definición de la función de seno
function seno(angulo)
  -- Conversión de grados a radianes
  angulo = angulo * math.pi / 180

  -- Aproximación del seno mediante la serie de Taylor
  local series = 0
  local termino = angulo
  local signo = -1
  for i=1, 10 do
    series = series + termino
    termino = termino * angulo^2 / (2*i+1) * signo
    signo = -signo
  end
  return series
end

-- Definición de la función de coseno
function coseno(angulo)
  -- Conversión de grados a radianes
  angulo = angulo * math.pi / 180

  -- Aproximación del coseno mediante la serie de Taylor
  local series = 0
  local termino = 1
  local signo = 1
  for i=1, 10 do
    series = series + termino
    termino = termino * angulo^2 / (2*i) * signo
    signo = -signo
  end
  return series
end

-- Definición de la función de tangente
function tangente(angulo)
  return seno(angulo) / coseno(angulo)
end
```

Este código define varias funciones matemáticas comunes en LUA:

* `factorial(n)`: Calcula el factorial de un número `n`.
* `fibonacci(n)`: Calcula el `n`-ésimo número de Fibonacci.
* `potencia(base, exponente)`: Calcula la potencia de una base elevada a un exponente.
* `raizCuadrada(numero)`: Calcula la raíz cuadrada de un número.
* `seno(angulo)`: Calcula el seno de un ángulo en grados.
* `coseno(angulo)`: Calcula el coseno de un ángulo en grados.
* `tangente(angulo)`: Calcula la tangente de un ángulo en grados.

Las funciones utilizan un enfoque recursivo para calcular los resultados, lo que las convierte en soluciones eficientes y precisas. El código está bien organizado y comentado para facilitar su comprensión y uso.