```lua
-- Definir una función para calcular el factorial de un número.
function factorial(n)
  if n == 0 then
    return 1
  else
    return n * factorial(n-1)
  end
end

-- Definir una función para encontrar el máximo común divisor de dos números.
function mcd(a, b)
  if b == 0 then
    return a
  else
    return mcd(b, a % b)
  end
end

-- Definir una función para encontrar el mínimo común múltiplo de dos números.
function mcm(a, b)
  return (a * b) / mcd(a, b)
end

-- Definir una función para generar una lista de números primos hasta un límite especificado.
function primos(n)
  local lista = {}
  for i = 2, n do
    if esPrimo(i) then
      table.insert(lista, i)
    end
  end
  return lista
end

-- Definir una función para verificar si un número es primo.
function esPrimo(n)
  if n <= 1 then
    return false
  end
  for i = 2, math.sqrt(n) do
    if n % i == 0 then
      return false
    end
  end
  return true
end

-- Definir una función para convertir un número a su representación en binario.
function aBinario(n)
  local binario = ""
  while n > 0 do
    binario = (n % 2) .. binario
    n = math.floor(n / 2)
  end
  return binario
end

-- Definir una función para convertir un número de su representación en binario a su valor decimal.
function deBinario(binario)
  local decimal = 0
  local potencia = 1
  for i = #binario, 1, -1 do
    decimal = decimal + (tonumber(binario:sub(i, i)) * potencia)
    potencia = potencia * 2
  end
  return decimal
end

-- Definir una función para ordenar una lista de números en orden ascendente.
function ordenar(lista)
  for i = 1, #lista do
    for j = i+1, #lista do
      if lista[i] > lista[j] then
        local temp = lista[i]
        lista[i] = lista[j]
        lista[j] = temp
      end
    end
  end
  return lista
end

-- Definir una función para buscar un elemento en una lista ordenada mediante búsqueda binaria.
function busquedaBinaria(lista, elemento)
  local inicio = 1
  local fin = #lista
  while inicio <= fin do
    local medio = math.floor((inicio + fin) / 2)
    if lista[medio] == elemento then
      return medio
    elseif lista[medio] < elemento then
      inicio = medio + 1
    else
      fin = medio - 1
    end
  end
  return -1
end

-- Definir una función para generar una matriz de n filas y m columnas.
function crearMatriz(n, m)
  local matriz = {}
  for i = 1, n do
    matriz[i] = {}
    for j = 1, m do
      matriz[i][j] = 0
    end
  end
  return matriz
end

-- Definir una función para sumar dos matrices de n filas y m columnas.
function sumarMatrices(a, b)
  if #a ~= #b or #a[1] ~= #b[1] then
    error("Las matrices deben tener las mismas dimensiones.")
  end

  local c = crearMatriz(#a, #a[1])
  for i = 1, #a do
    for j = 1, #a[1] do
      c[i][j] = a[i][j] + b[i][j]
    end
  end
  return c
end

-- Definir una función para multiplicar dos matrices de n filas y m columnas.
function multiplicarMatrices(a, b)
  if #a[1] ~= #b then
    error("Las matrices no se pueden multiplicar.")
  end

  local c = crearMatriz(#a, #b[1])
  for i = 1, #a do
    for j = 1, #b[1] do
      for k = 1, #a[1] do
        c[i][j] = c[i][j] + a[i][k] * b[k][j]
      end
    end
  end
  return c
end

-- Definir una función para calcular el determinante de una matriz cuadrada.
function determinante(a)
  if #a ~= #a[1] then
    error("La matriz no es cuadrada.")
  end

  if #a == 1 then
    return a[1][1]
  end

  local det = 0
  for i = 1, #a do
    local submatriz = {}
    for j = 2, #a do
      local fila = {}
      for k = 1, #a do
        if k ~= i then
          fila[k-1] = a[j][k]
        end
      end
      table.insert(submatriz, fila)
    end
    det = det + a[1][i] * determinante(submatriz) * (-1)^(i+1)
  end
  return det
end

-- Definir una función para calcular la inversa de una matriz cuadrada.
function inversa(a)
  if #a ~= #a[1] then
    error("La matriz no es cuadrada.")
  end

  local det = determinante(a)
  if det == 0 then
    error("La matriz no tiene inversa.")
  end

  local adjunta = {}
  for i = 1, #a do
    adjunta[i] = {}
    for j = 1, #a do
      local submatriz = {}
      for k = 1, #a do
        if k ~= i and j ~= 1 then
          local fila = {}
          for l = 1, #a do
            if l ~= j then
              fila[l-1] = a[k][l]
            end
          end
          table.insert(submatriz, fila)
        end
      end
      adjunta[i][j] = determinante(submatriz) * (-1)^(i+j)
    end
  end

  local inversa = {}
  for i = 1, #a do
    inversa[i] = {}
    for j = 1, #a do
      inversa[i][j] = adjunta[j][i] / det
    end
  end

  return inversa
end

```

Este código contiene varias funciones útiles para realizar operaciones matemáticas y trabajar con listas, matrices y otros tipos de datos. Las funciones incluyen cálculos básicos como factoriales, máximo común divisor y mínimo común múltiplo, así como funciones más avanzadas como generación de números primos, conversión entre binario y decimal, ordenación de listas, búsqueda binaria, creación y manipulación de matrices, cálculo de determinantes e inversas de matrices, entre otras.