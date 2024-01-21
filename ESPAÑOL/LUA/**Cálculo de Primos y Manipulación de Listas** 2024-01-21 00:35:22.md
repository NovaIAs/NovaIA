```lua
-- Módulo de Cálculo de Primos
-- Calcula números primos hasta un límite especificado

local function esPrimo(n)
  if n <= 1 then
    return false
  end
  for i = 2, math.floor(math.sqrt(n)) do
    if n % i == 0 then
      return false
    end
  end
  return true
end

local function generarPrimosHasta(n)
  local primos = {}
  for i = 2, n do
    if esPrimo(i) then
      primos[#primos + 1] = i
    end
  end
  return primos
end

-- Módulo de Manipulación de Listas
-- Implementa operaciones básicas de manipulación de listas

local function insertar(lista, elemento, posición)
  local nuevaLista = {}
  for i = 1, posición - 1 do
    nuevaLista[i] = lista[i]
  end
  nuevaLista[posición] = elemento
  for i = posición + 1, #lista + 1 do
    nuevaLista[i] = lista[i - 1]
  end
  return nuevaLista
end

local function eliminar(lista, posición)
  local nuevaLista = {}
  for i = 1, posición - 1 do
    nuevaLista[i] = lista[i]
  end
  for i = posición + 1, #lista do
    nuevaLista[i - 1] = lista[i]
  end
  return nuevaLista
end

-- Módulo Principal
-- Utiliza los módulos anteriores para resolver un problema específico

local primos = generarPrimosHasta(100)
print("Primos menores o iguales a 100:")
for _, primo in ipairs(primos) do
  print(primo)
end

local lista1 = {1, 2, 3, 4, 5}
local lista2 = insertar(lista1, 6, 3)
print("Lista 1 después de insertar el elemento 6 en la posición 3:")
for _, elemento in ipairs(lista2) do
  print(elemento)
end

local lista3 = eliminar(lista1, 2)
print("Lista 1 después de eliminar el elemento en la posición 2:")
for _, elemento in ipairs(lista3) do
  print(elemento)
end
```

Explicación del código:

* El código está dividido en tres módulos:

  * `Cálculo de Primos`: Este módulo proporciona una función para calcular números primos hasta un límite especificado.
  * `Manipulación de Listas`: Este módulo proporciona funciones para insertar y eliminar elementos de una lista.
  * `Principal`: Este módulo utiliza los dos módulos anteriores para resolver un problema específico, en este caso, calcular los números primos menores o iguales a 100 e insertar y eliminar elementos de una lista.

* El código principal comienza generando una lista de números primos menores o iguales a 100 utilizando la función `generarPrimosHasta`. Luego, imprime esta lista utilizando un bucle `for` anidado.

* A continuación, el código inserta el elemento 6 en la posición 3 de la lista `lista1` utilizando la función `insertar`. Luego, imprime la lista modificada.

* Finalmente, el código elimina el elemento en la posición 2 de la lista `lista1` utilizando la función `eliminar`. Luego, imprime la lista modificada.

* El código utiliza comentarios para explicar cada parte del código y facilitar su comprensión.