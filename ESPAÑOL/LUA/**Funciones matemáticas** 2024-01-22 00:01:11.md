```lua
-- Importación de librerías
local os = require("os")
local table = require("table")
local string = require("string")

-- Definición de funciones

-- Función para encontrar el máximo común divisor de dos números
function mcd(a, b)
  if b == 0 then
    return a
  end
  return mcd(b, a % b)
end

-- Función para encontrar el mínimo común múltiplo de dos números
function mcm(a, b)
  return (a * b) / mcd(a, b)
end

-- Función para generar una permutación de una lista
function permuta(lista)
  if #lista == 0 then
    return { { } }
  end

  local permutaciones = { }
  for i, elemento in ipairs(lista) do
    local sublista = table.remove(lista, i)
    for permutacion in permuta(sublista) do
      table.insert(permutacion, 1, elemento)
      table.insert(permutaciones, #permutaciones + 1, permutacion)
    end
    table.insert(lista, i, elemento)
  end

  return permutaciones
end

-- Función para generar una combinación de una lista
function combinacion(lista, k)
  if k == 0 then
    return { { } }
  end

  local combinaciones = { }
  for i, elemento in ipairs(lista) do
    local sublista = table.remove(lista, i)
    for combinacion in combinacion(sublista, k - 1) do
      table.insert(combinacion, #combinacion + 1, elemento)
      table.insert(combinaciones, #combinaciones + 1, combinacion)
    end
    table.insert(lista, i, elemento)
  end

  return combinaciones
end

-- Función para generar una subcadena de una cadena
function subcadena(cadena, inicio, fin)
  if inicio == nil then
    inicio = 1
  end
  if fin == nil then
    fin = #cadena
  end

  local subcadena = ""
  for i = inicio, fin do
    subcadena = subcadena .. string.sub(cadena, i, i)
  end

  return subcadena
end

-- Función para generar una lista de números primos
function primos(limite)
  local primos = { }
  for i = 2, limite do
    local esPrimo = true
    for divisor = 2, math.floor(math.sqrt(i)) do
      if i % divisor == 0 then
        esPrimo = false
        break
      end
    end

    if esPrimo then
      table.insert(primos, #primos + 1, i)
    end
  end

  return primos
end

-- Función para generar una lista de números perfectos
function perfectos(limite)
  local perfectos = { }
  for i = 2, limite do
    local sumaDivisores = 0
    for divisor = 1, math.floor(i / 2) do
      if i % divisor == 0 then
        sumaDivisores = sumaDivisores + divisor
      end
    end

    if sumaDivisores == i then
      table.insert(perfectos, #perfectos + 1, i)
    end
  end

  return perfectos
end

-- Función para generar una lista de números de Fibonacci
function fibonacci(limite)
  local fibonacci = { 0, 1 }
  while #fibonacci < limite do
    local siguiente = fibonacci[#fibonacci] + fibonacci[#fibonacci - 1]
    table.insert(fibonacci, #fibonacci + 1, siguiente)
  end

  return fibonacci
end

-- Función para generar una lista de números de Lucas
function lucas(limite)
  local lucas = { 2, 1 }
  while #lucas < limite do
    local siguiente = lucas[#lucas] + lucas[#lucas - 1]
    table.insert(lucas, #lucas + 1, siguiente)
  end

  return lucas
end

-- Función para generar una lista de números de Catalan
function catalan(limite)
  local catalan = { 1 }
  while #catalan < limite do
    local siguiente = 0
    for i = 1, #catalan do
      siguiente = siguiente + catalan[i] * catalan[#catalan + 1 - i]
    end
    table.insert(catalan, #catalan + 1, siguiente)
  end

  return catalan
end

-- Función para generar una lista de números de Bell
function bell(limite)
  local bell = { 1 }
  while #bell < limite do
    local siguiente = 0
    for i = 1, #bell do
      siguiente = siguiente + bell[i] * stirling2(limite - i)
    end
    table.insert(bell, #bell + 1, siguiente)
  end

  return bell
end

-- Función para generar una lista de números de Stirling de primer tipo
function stirling1(limite)
  local stirling1 = { { 0 } }
  for i = 2, limite do
    local fila = { }
    for j = 1, i do
      local elemento = 0
      if j == 1 or j == i then
        elemento = 1
      else
        for k = 1, i - 1 do
          elemento = elemento + stirling1[i - 1][k]
        end
        elemento = elemento - stirling1[i - 1][j - 1]
      end
      table.insert(fila, #fila