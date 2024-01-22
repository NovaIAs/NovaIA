```lua
-- Este código implementa un algoritmo de clasificación rápida en Lua.

-- Función para intercambiar dos elementos de una lista.
function swap(lista, i, j)
  local temp = lista[i]
  lista[i] = lista[j]
  lista[j] = temp
end

-- Función para particionar una lista en dos sublistas, según su valor relativo a un pivote.
function partition(lista, inicio, fin)
  -- Elegir un pivote al azar.
  local pivote = math.random(inicio, fin)

  -- Intercambiar el pivote con el último elemento de la lista.
  swap(lista, pivote, fin)

  -- Inicializar el índice de la partición.
  local i = inicio - 1

  -- Recorrer la lista desde el principio hasta el fin.
  for j = inicio, fin - 1 do
    -- Si el elemento actual es menor o igual al pivote.
    if lista[j] <= lista[fin] then
      -- Incrementar el índice de la partición.
      i = i + 1

      -- Intercambiar el elemento actual con el elemento en el índice de la partición.
      swap(lista, i, j)
    end
  end

  -- Intercambiar el pivote con el elemento en el índice de la partición.
  swap(lista, i + 1, fin)

  -- Devolver el índice de la partición.
  return i + 1
end

-- Función para ordenar una lista usando el algoritmo de clasificación rápida.
function quicksort(lista, inicio, fin)
  -- Si el inicio es menor o igual al fin.
  if inicio <= fin then
    -- Encontrar el índice de la partición.
    local pi = partition(lista, inicio, fin)

    -- Ordenar la sublista izquierda.
    quicksort(lista, inicio, pi - 1)

    -- Ordenar la sublista derecha.
    quicksort(lista, pi + 1, fin)
  end
end

-- Ejemplo de uso.
local lista = {5, 3, 1, 2, 4}
quicksort(lista, 1, #lista)
print(lista)
```

Explicación del código:

* La función `swap` intercambia dos elementos de una lista.
* La función `partition` particiona una lista en dos sublistas, según su valor relativo a un pivote.
* La función `quicksort` ordena una lista usando el algoritmo de clasificación rápida.
* El ejemplo de uso muestra cómo usar la función `quicksort` para ordenar una lista.