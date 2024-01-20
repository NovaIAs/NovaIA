```lua
-- **Este código genera un árbol binario de búsqueda a partir de una lista de números.**

-- **Definición de la función para crear el árbol binario de búsqueda.**
function crearArbol(lista)
  -- **Si la lista está vacía, el árbol es nulo.**
  if lista == nil or #lista == 0 then
    return nil
  end

  -- **El primer elemento de la lista será el valor del nodo raíz.**
  nodoRaiz = {valor = lista[1]}

  -- **Se recorre la lista desde el segundo elemento hasta el último.**
  for i = 2, #lista do
    -- **Se inserta el elemento en el árbol.**
    insertar(nodoRaiz, lista[i])
  end

  -- **Se devuelve el nodo raíz del árbol.**
  return nodoRaiz
end

-- **Definición de la función para insertar un elemento en el árbol binario de búsqueda.**
function insertar(nodo, valor)
  -- **Si el valor es menor que el valor del nodo actual, se inserta en el subárbol izquierdo.**
  if valor < nodo.valor then
    -- **Si el subárbol izquierdo es nulo, se crea un nuevo nodo con el valor.**
    if nodo.izquierdo == nil then
      nodo.izquierdo = {valor = valor}
    else
      -- **Si el subárbol izquierdo no es nulo, se llama recursivamente a la función insertar.**
      insertar(nodo.izquierdo, valor)
    end
  -- **Si el valor es mayor o igual que el valor del nodo actual, se inserta en el subárbol derecho.**
  else
    -- **Si el subárbol derecho es nulo, se crea un nuevo nodo con el valor.**
    if nodo.derecho == nil then
      nodo.derecho = {valor = valor}
    else
      -- **Si el subárbol derecho no es nulo, se llama recursivamente a la función insertar.**
      insertar(nodo.derecho, valor)
    end
  end
end

-- **Ejemplo de uso.**
lista = {1, 3, 5, 7, 9, 11, 13, 15}
arbol = crearArbol(lista)
```

**Explicación del código:**

* La función `crearArbol()` toma una lista de números como argumento y devuelve el nodo raíz del árbol binario de búsqueda creado a partir de la lista.
* La función `insertar()` toma un nodo y un valor como argumentos e inserta el valor en el árbol binario de búsqueda.
* El ejemplo de uso del código crea un árbol binario de búsqueda a partir de una lista de números y luego imprime el árbol.