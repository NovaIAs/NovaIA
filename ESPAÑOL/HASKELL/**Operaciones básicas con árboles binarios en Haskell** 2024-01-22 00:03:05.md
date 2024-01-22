```haskell
-- Definimos el tipo de datos Arbol para representar árboles binarios.
data Arbol a = Vacio | Nodo a (Arbol a) (Arbol a)

-- Definimos la función árbol_desde_lista, que crea un árbol binario a partir de una lista de elementos.
árbol_desde_lista :: [a] -> Arbol a
árbol_desde_lista [] = Vacio
árbol_desde_lista (x:xs) = Nodo x (árbol_desde_lista (take (div (length xs) 2) xs)) (árbol_desde_lista (drop (div (length xs) 2) xs))

-- Definimos la función profundidad, que calcula la profundidad de un árbol binario.
profundidad :: Arbol a -> Int
profundidad Vacio = 0
profundidad (Nodo _ izq der) = 1 + max (profundidad izq) (profundidad der)

-- Definimos la función anchura, que calcula la anchura de un árbol binario.
anchura :: Arbol a -> Int
anchura Vacio = 0
anchura (Nodo _ izq der) = max (anchura izq) (anchura der) + 1

-- Definimos la función hojas, que devuelve una lista con las hojas de un árbol binario.
hojas :: Arbol a -> [a]
hojas Vacio = []
hojas (Nodo x izq der) = hojas izq ++ hojas der ++ [x]

-- Definimos la función buscar, que busca un elemento en un árbol binario y devuelve True si lo encuentra, False en caso contrario.
buscar :: Eq a => a -> Arbol a -> Bool
buscar _ Vacio = False
buscar x (Nodo y izq der) = x == y || buscar x izq || buscar x der

-- Definimos la función insertar, que inserta un elemento en un árbol binario.
insertar :: Ord a => a -> Arbol a -> Arbol a
insertar x Vacio = Nodo x Vacio Vacio
insertar x (Nodo y izq der)
  | x <= y = Nodo y (insertar x izq) der
  | otherwise = Nodo y izq (insertar x der)

-- Definimos la función eliminar, que elimina un elemento de un árbol binario.
eliminar :: Ord a => a -> Arbol a -> Arbol a
eliminar x Vacio = Vacio
eliminar x (Nodo y izq der)
  | x < y = Nodo y (eliminar x izq) der
  | x > y = Nodo y izq (eliminar x der)
  | otherwise = fusionar izq der

-- Definimos la función fusionar, que fusiona dos árboles binarios en uno solo.
fusionar :: Arbol a -> Arbol a -> Arbol a
fusionar izq der = árbol_desde_lista (hojas izq ++ hojas der)

-- Definimos la función imprimir, que imprime un árbol binario en consola.
imprimir :: Arbol a -> IO ()
imprimir Vacio = putStrLn "Vacio"
imprimir (Nodo x izq der) = do
  putStrLn (show x)
  imprimir izq
  imprimir der
```

Explicación:

* El tipo de datos `Arbol a` representa un árbol binario de elementos de tipo `a`. Puede ser vacío (Vacio) o contener un valor `a` y dos subárboles (Nodo a izq der).
* La función `árbol_desde_lista` crea un árbol binario a partir de una lista de elementos.
* La función `profundidad` calcula la profundidad de un árbol binario.
* La función `anchura` calcula la anchura de un árbol binario.
* La función `hojas` devuelve una lista con las hojas de un árbol binario.
* La función `buscar` busca un elemento en un árbol binario y devuelve True si lo encuentra, False en caso contrario.
* La función `insertar` inserta un elemento en un árbol binario.
* La función `eliminar` elimina un elemento de un árbol binario.
* La función `fusionar` fusiona dos árboles binarios en uno solo.
* La función `imprimir` imprime un árbol binario en consola.

Este código es un ejemplo de código complejo en Haskell que realiza una variedad de operaciones en árboles binarios.