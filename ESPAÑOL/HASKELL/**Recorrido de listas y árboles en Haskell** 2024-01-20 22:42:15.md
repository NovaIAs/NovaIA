```haskell
{-
    Definición de los tipos de datos
-}
data Lista a = Vacía | Nodo a (Lista a)

data Árbol a = Hoja a | Rama (Árbol a) (Árbol a)

{-
    Definición de las funciones recursivas
-}
recorrerLista :: Lista a -> [a]
recorrerLista Vacía = []
recorrerLista (Nodo x xs) = x : recorrerLista xs

recorrerÁrbol :: Árbol a -> [a]
recorrerÁrbol (Hoja x) = [x]
recorrerÁrbol (Rama l r) = recorrerÁrbol l ++ recorrerÁrbol r

{-
    Aplicación de las funciones recursivas
-}

-- Definición de una lista de enteros
listaEnteros = Nodo 1 (Nodo 2 (Nodo 3 (Nodo 4 (Nodo 5 Vacía))))

-- Aplicación de la función recorrerLista a la lista de enteros
resultadoLista = recorrerLista listaEnteros

-- Definición de un árbol binario de números
arbolBinario = Rama (Hoja 1) (Rama (Hoja 2) (Hoja 3))

-- Aplicación de la función recorrerÁrbol al árbol binario de números
resultadoÁrbol = recorrerÁrbol arbolBinario

{-
    Explicación del código
-}

* Las funciones `recorrerLista` y `recorrerÁrbol` son funciones recursivas que recorren una lista y un árbol, respectivamente, y devuelven una lista de los elementos contenidos en la estructura de datos.
* La función `recorrerLista` recorre la lista de izquierda a derecha, mientras que la función `recorrerÁrbol` recorre el árbol en profundidad, visitando primero el nodo raíz, luego el subárbol izquierdo y finalmente el subárbol derecho.
* La aplicación de las funciones `recorrerLista` y `recorrerÁrbol` a las estructuras de datos definidas en el código devuelve las listas `resultadoLista` y `resultadoÁrbol`, que contienen los elementos de la lista y del árbol, respectivamente.

Este código es complejo y difícil de repetir porque utiliza recursividad para recorrer estructuras de datos complejas, como listas y árboles. La recursividad es una técnica poderosa que se puede utilizar para resolver una amplia variedad de problemas, pero también puede ser difícil de entender y depurar.
```