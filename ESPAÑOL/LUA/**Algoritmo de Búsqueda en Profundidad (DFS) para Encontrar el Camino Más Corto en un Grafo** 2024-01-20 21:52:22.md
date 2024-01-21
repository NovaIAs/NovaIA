```lua
-- Este código implementa un algoritmo que encuentra el camino más corto entre dos nodos en un grafo.

-- Primero, definimos una función que crea un grafo a partir de una lista de aristas.
function crear_grafo(aristas)
  -- Inicializamos el grafo como una tabla vacía.
  grafo = {}

  -- Para cada arista en la lista de aristas, añadimos los nodos de la arista al grafo, si no están ya presentes.
  for _, arista in pairs(aristas) do
    nodo1, nodo2 = unpack(arista)
    grafo[nodo1] = grafo[nodo1] or {}
    grafo[nodo2] = grafo[nodo2] or {}

    -- Añadimos la arista al grafo.
    grafo[nodo1][nodo2] = true
    grafo[nodo2][nodo1] = true
  end

  -- Devolvemos el grafo.
  return grafo
end

-- A continuación, definimos una función que realiza una búsqueda en profundidad en el grafo para encontrar el camino más corto entre dos nodos.
function dfs(grafo, nodo_inicial, nodo_objetivo, camino_actual)
  -- Si el nodo actual es el nodo objetivo, devolvemos el camino actual.
  if nodo_actual == nodo_objetivo then
    return camino_actual
  end

  -- Marcamos el nodo actual como visitado.
  grafo[nodo_actual] = nil

  -- Para cada nodo adyacente al nodo actual, llamamos recursivamente a la función dfs con el nodo adyacente como nodo actual y añadimos el nodo adyacente al camino actual.
  for nodo_adyacente, _ in pairs(grafo[nodo_actual]) do
    if grafo[nodo_adyacente] then
      camino_actual = camino_actual .. " -> " .. nodo_adyacente
      resultado = dfs(grafo, nodo_adyacente, nodo_objetivo, camino_actual)
      if resultado then
        return resultado
      end
    end
  end

  -- Devolvemos nil si no se encuentra ningún camino.
  return nil
end

-- Finalmente, definimos una función que encuentra el camino más corto entre dos nodos en un grafo llamando a la función dfs.
function encontrar_camino_mas_corto(grafo, nodo_inicial, nodo_objetivo)
  -- Llamamos a la función dfs con el nodo inicial, el nodo objetivo y un camino actual vacío.
  resultado = dfs(grafo, nodo_inicial, nodo_objetivo, "")

  -- Devolvemos el camino más corto.
  return resultado
end

-- Ejemplo de uso:
aristas = {
  {1, 2},
  {1, 3},
  {2, 3},
  {2, 4},
  {3, 4},
  {3, 5},
  {4, 5}
}

grafo = crear_grafo(aristas)

camino_mas_corto = encontrar_camino_mas_corto(grafo, 1, 5)

print(camino_mas_corto)
```

Explicación del código:

- La función `crear_grafo` toma una lista de aristas y crea un grafo representado por una tabla, donde cada nodo es una clave y el valor asociado es una tabla de los nodos adyacentes.


- La función `dfs` realiza una búsqueda en profundidad en el grafo para encontrar el camino más corto entre dos nodos. La función toma cuatro argumentos: el grafo, el nodo inicial, el nodo objetivo y el camino actual. La función marca el nodo actual como visitado y luego llama recursivamente a sí misma con cada nodo adyacente al nodo actual como nodo actual y añade el nodo adyacente al camino actual. Si el nodo actual es el nodo objetivo, la función devuelve el camino actual.


- La función `encontrar_camino_mas_corto` llama a la función `dfs` con el nodo inicial, el nodo objetivo y un camino actual vacío y devuelve el resultado.


- En el ejemplo de uso, se crea un grafo a partir de una lista de aristas y luego se utiliza la función `encontrar_camino_mas_corto` para encontrar el camino más corto entre los nodos 1 y 5. El resultado es impreso en la consola.