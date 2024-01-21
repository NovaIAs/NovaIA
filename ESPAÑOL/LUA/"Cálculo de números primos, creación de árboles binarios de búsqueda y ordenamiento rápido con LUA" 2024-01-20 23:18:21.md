```lua
-- Este es un código complejo en LUA que realiza varias tareas, incluyendo el cálculo de números primos, la creación de un árbol binario de búsqueda y la implementación de un algoritmo de ordenamiento rápido.

-- Función para calcular los números primos hasta un número dado
function calcular_primos(n)
  local primos = {}
  for i = 2, n do
    local es_primo = true
    for j = 2, math.floor(math.sqrt(i)) do
      if i % j == 0 then
        es_primo = false
        break
      end
    end
    if es_primo then
      primos[#primos + 1] = i
    end
  end
  return primos
end

-- Función para crear un árbol binario de búsqueda a partir de una lista de números
function crear_arbol(lista)
  if #lista == 0 then
    return nil
  end

  local nodo_raiz = lista[1]
  local arbol_izquierdo = crear_arbol(table.sub(lista, 2, #lista / 2))
  local arbol_derecho = crear_arbol(table.sub(lista, #lista / 2 + 1, #lista))

  return {nodo_raiz, arbol_izquierdo, arbol_derecho}
end

-- Función para implementar el algoritmo de ordenamiento rápido
function ordenamiento_rapido(lista)
  if #lista <= 1 then
    return lista
  end

  local pivote = lista[math.floor(#lista / 2)]
  local lista_izquierda = {}
  local lista_derecha = {}
  for i = 1, #lista do
    if lista[i] < pivote then
      lista_izquierda[#lista_izquierda + 1] = lista[i]
    elseif lista[i] > pivote then
      lista_derecha[#lista_derecha + 1] = lista[i]
    end
  end

  return table.concat({ordenamiento_rapido(lista_izquierda), {pivote}, ordenamiento_rapido(lista_derecha)})
end

-- Imprimir los resultados
print("Números primos hasta 100:")
print(table.concat(calcular_primos(100), ", "))

print("\nÁrbol binario de búsqueda:")
local arbol = crear_arbol({1, 2, 3, 4, 5, 6, 7, 8, 9, 10})
print(arbol)

print("\nLista ordenada:")
local lista = {10, 9, 8, 7, 6, 5, 4, 3, 2, 1}
print(table.concat(ordenamiento_rapido(lista), ", "))
```

Explicación del código:

* La función `calcular_primos` calcula los números primos hasta un número dado utilizando el método de la criba de Eratóstenes.
* La función `crear_arbol` crea un árbol binario de búsqueda a partir de una lista de números.
* La función `ordenamiento_rapido` implementa el algoritmo de ordenamiento rápido para ordenar una lista de números.
* El código imprime los resultados de las tres funciones en la consola.