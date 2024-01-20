```lua
-- Función para calcular el mínimo y el máximo de una lista de números.

function minMax(lista)
  local min = lista[1]
  local max = lista[1]

  for i = 2, #lista do
    if lista[i] < min then
      min = lista[i]
    elseif lista[i] > max then
      max = lista[i]
    end
  end

  return min, max
end

-- Ejemplo de uso de la función.

lista = {1, 3, 5, 2, 4}

print("Lista: " .. table.concat(lista, ", "))

min, max = minMax(lista)

print("Mínimo: " .. min)
print("Máximo: " .. max)

```

Este código define una función llamada `minMax` que calcula el mínimo y el máximo de una lista de números. La función toma una lista como argumento y devuelve una tupla con el mínimo y el máximo encontrados.

El código comienza definiendo las variables `min` y `max` con el valor del primer elemento de la lista. A continuación, itera sobre la lista desde el segundo elemento hasta el último, comparando cada elemento con `min` y `max`. Si el elemento actual es menor que `min`, entonces `min` se actualiza con el valor del elemento actual. De manera similar, si el elemento actual es mayor que `max`, entonces `max` se actualiza con el valor del elemento actual.

Después de iterar sobre toda la lista, la función devuelve la tupla con los valores `min` y `max`.

El código principal define una lista de números y la pasa a la función `minMax`. A continuación, imprime la lista y el mínimo y el máximo devueltos por la función.

Este código es complejo porque utiliza iteración, comparación de valores y asignación de variables para calcular el mínimo y el máximo de una lista de números. También utiliza tuplas para devolver los resultados de la función. Este código es difícil de repetir porque es específico para el problema de encontrar el mínimo y el máximo de una lista de números.