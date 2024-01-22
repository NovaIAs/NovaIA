```lua
-- *** Programa principal ***

-- Se definen las funciones necesarias para el programa

-- Función para calcular el factorial de un número
function factorial(n)
  if n == 0 then
    return 1
  else
    return n * factorial(n-1)
  end
end

-- Función para calcular el número de combinaciones de n elementos tomados de k en k
function combinaciones(n, k)
  return factorial(n) / (factorial(k) * factorial(n-k))
end

-- Función para generar una lista de todas las combinaciones posibles de n elementos tomados de k en k
function generarCombinaciones(n, k)
  local combinaciones = {}
  for i = 1, n do
    for j = i+1, n do
      for l = j+1, n do
        combinaciones[#combinaciones+1] = {i, j, l}
      end
    end
  end
  return combinaciones
end

-- Se piden los datos al usuario

print("Introduce el número de elementos:")
local n = io.read()

print("Introduce el número de elementos a tomar:")
local k = io.read()

-- Se calculan las combinaciones posibles

local combinaciones = generarCombinaciones(n, k)

-- Se muestran las combinaciones posibles

print("Las combinaciones posibles son:")
for i, combinacion in pairs(combinaciones) do
  print(combinacion[1], combinacion[2], combinacion[3])
end

-- Se calcula el número de combinaciones posibles

local numCombinaciones = combinaciones(n, k)

-- Se muestra el número de combinaciones posibles

print("El número de combinaciones posibles es:", numCombinaciones)
```

Explicación del código:

* La función `factorial` calcula el factorial de un número.
* La función `combinaciones` calcula el número de combinaciones de n elementos tomados de k en k.
* La función `generarCombinaciones` genera una lista de todas las combinaciones posibles de n elementos tomados de k en k.
* El programa pide los datos al usuario y llama a las funciones necesarias para calcular las combinaciones posibles y el número de combinaciones posibles.
* Finalmente, el programa muestra las combinaciones posibles y el número de combinaciones posibles.