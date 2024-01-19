```lua
-- Definición de la función "suma" que toma dos números y devuelve su suma.
function suma(a, b)
  return a + b
end

-- Definición de la función "resta" que toma dos números y devuelve su resta.
function resta(a, b)
  return a - b
end

-- Definición de la función "multiplicacion" que toma dos números y devuelve su multiplicación.
function multiplicacion(a, b)
  return a * b
end

-- Definición de la función "division" que toma dos números y devuelve su división.
function division(a, b)
  if b == 0 then
    error("No se puede dividir por cero.")
  end
  return a / b
end

-- Definición de la función "potencia" que toma dos números y devuelve el primero elevado al segundo.
function potencia(a, b)
  if b < 0 then
    error("El exponente no puede ser negativo.")
  end
  local resultado = 1
  for i = 1, b do
    resultado = resultado * a
  end
  return resultado
end

-- Definición de la función "factorial" que toma un número y devuelve su factorial.
function factorial(n)
  if n < 0 then
    error("El número no puede ser negativo.")
  end
  local resultado = 1
  for i = 1, n do
    resultado = resultado * i
  end
  return resultado
end

-- Definición de la función "fibonacci" que toma un número y devuelve el n-ésimo número de Fibonacci.
function fibonacci(n)
  if n < 0 then
    error("El número no puede ser negativo.")
  end
  local a = 0
  local b = 1
  for i = 2, n do
    local c = a + b
    a = b
    b = c
  end
  return b
end

-- Definición de la función "primo" que toma un número y devuelve si es primo o no.
function primo(n)
  if n < 2 then
    return false
  end
  for i = 2, n - 1 do
    if n % i == 0 then
      return false
    end
  end
  return true
end

-- Definición de la función "lista_primos" que toma un número y devuelve una lista de los números primos hasta ese número.
function lista_primos(n)
  local lista_primos = {}
  for i = 2, n do
    if primo(i) then
      lista_primos[#lista_primos + 1] = i
    end
  end
  return lista_primos
end

-- Definición de la función "mcd" que toma dos números y devuelve su máximo común divisor.
function mcd(a, b)
  while b ~= 0 do
    local temp = b
    b = a % b
    a = temp
  end
  return a
end

-- Definición de la función "mcm" que toma dos números y devuelve su mínimo común múltiplo.
function mcm(a, b)
  return (a * b) / mcd(a, b)
end

-- Definición de la función "ordenar" que toma una lista y devuelve una nueva lista ordenada.
function ordenar(lista)
  local lista_ordenada = {}
  for i = 1, #lista do
    local menor = lista[i]
    local indice_menor = i
    for j = i + 1, #lista do
      if lista[j] < menor then
        menor = lista[j]
        indice_menor = j
      end
    end
    lista[indice_menor] = lista[i]
    lista[i] = menor
  end
  return lista_ordenada
end

-- Definición de la función "buscar" que toma una lista y un elemento y devuelve el índice de la primera aparición del elemento en la lista, o -1 si no se encuentra.
function buscar(lista, elemento)
  for i = 1, #lista do
    if lista[i] == elemento then
      return i
    end
  end
  return -1
end

-- Definición de la función "eliminar" que toma una lista y un elemento y devuelve una nueva lista con el elemento eliminado, o la lista original si el elemento no se encuentra.
function eliminar(lista, elemento)
  local indice = buscar(lista, elemento)
  if indice == -1 then
    return lista
  end
  local lista_nueva = {}
  for