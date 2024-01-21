```haskell
-- Definimos un tipo de dato para representar un árbol binario de búsqueda.
data ArbolBinario a = Vacío | Nodo a (ArbolBinario a) (ArbolBinario a)

-- Función para insertar un elemento en un árbol binario de búsqueda.
insertar :: Ord a => a -> ArbolBinario a -> ArbolBinario a
insertar x arbol =
    case arbol of
        Vacío -> Nodo x Vacío Vacío
        Nodo y izq der ->
            if x < y then
                Nodo y (insertar x izq) der
            else
                Nodo y izq (insertar x der)

-- Función para buscar un elemento en un árbol binario de búsqueda.
buscar :: Ord a => a -> ArbolBinario a -> Bool
buscar x arbol =
    case arbol of
        Vacío -> False
        Nodo y izq der ->
            if x == y then
                True
            else if x < y then
                buscar x izq
            else
                buscar x der

-- Función para eliminar un elemento de un árbol binario de búsqueda.
eliminar :: Ord a => a -> ArbolBinario a -> ArbolBinario a
eliminar x arbol =
    case arbol of
        Vacío -> Vacío
        Nodo y izq der ->
            if x == y then
                -- Si el nodo a eliminar no tiene hijos, simplemente lo eliminamos.
                if izq == Vacío && der == Vacío then
                    Vacío
                -- Si el nodo a eliminar tiene un único hijo, lo promovemos.
                else if izq == Vacío then
                    der
                else if der == Vacío then
                    izq
                -- Si el nodo a eliminar tiene dos hijos, buscamos el mayor de los
                -- elementos del subárbol izquierdo y lo promovemos.
                else
                    let antecesor = mayor izq
                    in Nodo (valor antecesor) (eliminar (valor antecesor) izq) der
            else if x < y then
                Nodo y (eliminar x izq) der
            else
                Nodo y izq (eliminar x der)

-- Función para obtener el mayor de los elementos de un árbol binario de búsqueda.
mayor :: Ord a => ArbolBinario a -> ArbolBinario a
mayor arbol =
    case arbol of
        Vacío -> Vacío
        Nodo y Vacío _ -> arbol
        Nodo y izq der -> mayor der

-- Función para obtener el valor de un nodo de un árbol binario de búsqueda.
valor :: ArbolBinario a -> a
valor (Nodo x _ _) = x

-- Función para imprimir un árbol binario de búsqueda.
imprimir :: Show a => ArbolBinario a -> IO ()
imprimir arbol =
    do
        mapM_ (\x -> print x) (recorrido arbol)
        putStrLn ""

-- Función para obtener el recorrido en orden de un árbol binario de búsqueda.
recorrido :: Ord a => ArbolBinario a -> [a]
recorrido arbol =
    case arbol of
        Vacío -> []
        Nodo y izq der -> recorrido izq ++ [y] ++ recorrido der
```

Explicación del código:

* El tipo de dato `ArbolBinario` representa un árbol binario de búsqueda. Puede estar vacío (`Vacío`) o ser un nodo (`Nodo`) con un valor (`a`) y dos subárboles (`izq` y `der`).
* La función `insertar` inserta un elemento en un árbol binario de búsqueda. Si el árbol está vacío, crea un nuevo nodo con el elemento. Si el árbol no está vacío, compara el elemento con el valor del nodo actual. Si el elemento es menor que el valor del nodo actual, se inserta en el subárbol izquierdo. Si el elemento es mayor que el valor del nodo actual, se inserta en el subárbol derecho.
* La función `buscar` busca un elemento en un árbol binario de búsqueda. Si el árbol está vacío, devuelve `False`. Si el árbol no está vacío, compara el elemento con el valor del nodo actual. Si el elemento es igual al valor del nodo actual, devuelve `True`. Si el elemento es menor que el valor del nodo actual, busca el elemento en el subárbol izquierdo. Si el elemento es mayor que el valor del nodo actual, busca el elemento en el subárbol derecho.
* La función `eliminar` elimina un elemento de un árbol binario de búsqueda. Si el árbol está vacío, devuelve un árbol vacío. Si el árbol no está vacío, compara el elemento con el valor del nodo actual. Si el elemento es igual al valor del nodo actual, elimina el nodo actual. Si el elemento es menor que el valor del nodo actual, elimina el elemento del subárbol izquierdo. Si el elemento es mayor que el valor del nodo actual, elimina el elemento del subárbol derecho.
* La función `mayor` obtiene el mayor de los elementos de un árbol binario de búsqueda. Si el árbol está vacío, devuelve un árbol vacío. Si el árbol no está vacío, busca el mayor de los elementos del subárbol derecho.
* La función `valor` obtiene el valor de un nodo de un árbol binario de búsqueda.
* La función `imprimir` imprime un árbol binario de búsqueda. Utiliza la función `recorrido` para obtener el recorrido en orden del árbol y luego imprime cada elemento del recorrido.
* La función `recorrido` obtiene el recorrido en orden de un árbol binario de búsqueda. Si el árbol está vacío, devuelve una lista vacía. Si el árbol no está vacío, devuelve el recorrido en orden del subárbol izquierdo, seguido del valor del nodo actual, seguido del recorrido en orden del subárbol derecho.