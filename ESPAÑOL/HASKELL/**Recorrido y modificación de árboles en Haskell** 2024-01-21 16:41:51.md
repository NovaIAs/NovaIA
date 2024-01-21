```haskell
-- Definición de tipos

data Arbol a = Hoja a | Rama (Arbol a) (Arbol a)
  deriving (Show, Eq)

-- Funciones auxiliares

-- Recorre un árbol en profundidad, aplicando una función a cada nodo
recorrerEnProfundidad :: (a -> b) -> Arbol a -> [b]
recorrerEnProfundidad f (Hoja a) = [f a]
recorrerEnProfundidad f (Rama izq der) = recorrerEnProfundidad f izq ++ recorrerEnProfundidad f der

-- Recorre un árbol en amplitud, aplicando una función a cada nodo
recorrerEnAmplitud :: (a -> b) -> Arbol a -> [b]
recorrerEnAmplitud f arbol = recorrerEnAmplitud' [arbol]
  where
    recorrerEnAmplitud' :: [Arbol a] -> [b]
    recorrerEnAmplitud' [] = []
    recorrerEnAmplitud' (arbol:arboles) = f arbol : recorrerEnAmplitud' arboles

-- Creación de árboles

arbolBinario :: a -> Arbol a
arbolBinario a = Rama (Hoja a) (Hoja a)

arbolCompleto :: [a] -> Arbol a
arbolCompleto [] = Hoja ()
arbolCompleto (a:as) = Rama (arbolCompleto as) (arbolCompleto as)

-- Ejemplos de árboles

arbolBinario1 :: Arbol Int
arbolBinario1 = arbolBinario 1

arbolCompleto1 :: Arbol Char
arbolCompleto1 = arbolCompleto "abcdefghij"

-- Aplicación de funciones a árboles

-- Suma los valores de las hojas de un árbol
sumaHojas :: Arbol Int -> Int
sumaHojas (Hoja a) = a
sumaHojas (Rama izq der) = sumaHojas izq + sumaHojas der

-- Invierte un árbol
invertirArbol :: Arbol a -> Arbol a
invertirArbol (Hoja a) = Hoja a
invertirArbol (Rama izq der) = Rama (invertirArbol der) (invertirArbol izq)

-- Muestra un árbol en consola
mostrarArbol :: (Show a) => Arbol a -> IO ()
mostrarArbol arbol = putStrLn $ recorrerEnProfundidad show arbol

-- Ejecución del programa

main :: IO ()
main = do
  print $ sumaHojas arbolBinario1
  print $ invertirArbol arbolCompleto1
  mostrarArbol arbolBinario1
  mostrarArbol arbolCompleto1
```

**Explicación del código:**

* **Definición de tipos:**

    * `Arbol a` es el tipo de datos que representa un árbol. Un árbol puede ser una hoja (con un solo valor) o una rama (con dos subárboles).

* **Funciones auxiliares:**

    * `recorrerEnProfundidad` recorre un árbol en profundidad, aplicando una función a cada nodo.
    * `recorrerEnAmplitud` recorre un árbol en amplitud, aplicando una función a cada nodo.

* **Creación de árboles:**

    * `arbolBinario` crea un árbol binario a partir de un valor.
    * `arbolCompleto` crea un árbol completo a partir de una lista de valores.

* **Ejemplos de árboles:**

    * `arbolBinario1` es un árbol binario con un solo valor.
    * `arbolCompleto1` es un árbol completo con 10 valores.

* **Aplicación de funciones a árboles:**

    * `sumaHojas` suma los valores de las hojas de un árbol.
    * `invertirArbol` invierte un árbol.
    * `mostrarArbol` muestra un árbol en consola.

* **Ejecución del programa:**

    * La función `main` ejecuta el programa.
    * El programa imprime el resultado de aplicar las funciones `sumaHojas` e `invertirArbol` a los árboles `arbolBinario1` y `arbolCompleto1`.
    * El programa también muestra los árboles `arbolBinario1` y `arbolCompleto1` en consola.