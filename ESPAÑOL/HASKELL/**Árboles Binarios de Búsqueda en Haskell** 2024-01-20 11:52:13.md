```haskell
-- Módulo principal

module Principal where

-- Importar módulos necesarios

import Data.List (nub, sort)
import Data.Maybe (fromMaybe)
import System.IO (readFile)

-- Definir tipos de datos

data Arbol a = Hoja a | Rama (Arbol a) (Arbol a)
  deriving (Eq, Ord, Show)

-- Definir funciones auxiliares

agregar :: Ord a => a -> Arbol a -> Arbol a
agregar x Hoja = Rama (Hoja x) Hoja
agregar x (Rama izq der)
  | x <= raiz izq = Rama (agregar x izq) der
  | otherwise = Rama izq (agregar x der)
  where raiz = root izq

crearArbol :: Ord a => [a] -> Arbol a
crearArbol = foldl agregar Hoja

recorrerPreorden :: Arbol a -> [a]
recorrerPreorden Hoja = []
recorrerPreorden (Rama izq der) = root izq : recorrerPreorden izq ++ recorrerPreorden der
  where root = root izq

recorrerInorden :: Arbol a -> [a]
recorrerInorden Hoja = []
recorrerInorden (Rama izq der) = recorrerInorden izq ++ [root izq] ++ recorrerInorden der
  where root = root izq

recorrerPostorden :: Arbol a -> [a]
recorrerPostorden Hoja = []
recorrerPostorden (Rama izq der) = recorrerPostorden izq ++ recorrerPostorden der ++ [root izq]
  where root = root izq

buscar :: Ord a => a -> Arbol a -> Maybe a
buscar x Hoja = Nothing
buscar x (Rama izq der)
  | x == raiz izq = Just raiz izq
  | x < raiz izq = buscar x izq
  | otherwise = buscar x der
  where raiz = root izq

eliminar :: Ord a => a -> Arbol a -> Arbol a
eliminar x Hoja = Hoja
eliminar x (Rama izq der)
  | x == raiz izq = fromMaybe Hoja (minimo der)
  | x < raiz izq = Rama (eliminar x izq) der
  | otherwise = Rama izq (eliminar x der)
  where raiz = root izq
        minimo = minimoArbol

minimoArbol :: Ord a => Arbol a -> Maybe a
minimoArbol Hoja = Nothing
minimoArbol (Rama izq _) = minimoArbol izq

maximoArbol :: Ord a => Arbol a -> Maybe a
maximoArbol Hoja = Nothing
maximoArbol (Rama _ der) = maximoArbol der

alturaArbol :: Arbol a -> Int
alturaArbol Hoja = 0
alturaArbol (Rama izq der) = 1 + max (alturaArbol izq) (alturaArbol der)

esBalanceado :: Ord a => Arbol a -> Bool
esBalanceado Hoja = True
esBalanceado (Rama izq der) = abs (alturaArbol izq - alturaArbol der) <= 1 && esBalanceado izq && esBalanceado der

-- Definir la función principal

main :: IO ()
main = do
  -- Leer los datos de entrada
  datos <- readFile "datos.txt"

  -- Crear el árbol binario de búsqueda
  arbol <- return $ crearArbol [1, 2, 3, 4, 5, 6, 7]

  -- Imprimir el árbol en preorden
  putStrLn "Recorrido preorden:"
  print $ recorrerPreorden arbol

  -- Imprimir el árbol en inorden
  putStrLn "Recorrido inorden:"
  print $ recorrerInorden arbol

  -- Imprimir el árbol en postorden
  putStrLn "Recorrido postorden:"
  print $ recorrerPostorden arbol

  -- Buscar un valor en el árbol
  valor <- return 4
  let resultado = buscar valor arbol
  putStrLn $ "El valor " ++ show valor ++ " se encuentra en el árbol: " ++ show resultado

  -- Eliminar un valor del árbol
  valor <- return 5
  let arbolEliminado = eliminar valor arbol
  putStrLn $ "El árbol después de eliminar el valor " ++ show valor ++ ":"
  print arbolEliminado

  -- Comprobar si el árbol está balanceado
  let balanceado = esBalanceado arbol
  putStrLn $ "El árbol está balanceado: " ++ show balanceado
```

**Explicación del código:**

El código anterior implementa un árbol binario de búsqueda en Haskell. El código está dividido en varias partes:

* **Tipos de datos:**

```haskell
data Arbol a = Hoja a | Rama (Arbol a) (Arbol a)
  deriving (Eq, Ord, Show)
```

El tipo de datos `Arbol a` representa un árbol binario de búsqueda. Puede ser una hoja, que contiene un valor `a`, o una rama, que contiene dos subárboles, el izquierdo y el derecho.

* **Funciones auxiliares:**

```haskell
agregar :: Ord a => a -> Arbol a -> Arbol a
crearArbol :: Ord a => [a] -> Arbol a
recorrerPreorden :: Arbol a -> [a]
recorrerInorden :: Arbol a -> [a]
recorrerPostorden :: Arbol a -> [a]
buscar :: Ord a => a -> Arbol a -> Maybe a
eliminar :: Ord a => a -> Arbol a -> Arbol a
minimoArbol :: Ord a => Arbol a -> Maybe a
maximoArbol :: Ord a => Arbol a -> Maybe a
alturaArbol :: Arbol a -> Int
esBalanceado :: Ord a => Arbol a -> Bool
```

Estas funciones auxiliares se utilizan para manipular el árbol binario de búsqueda. Por ejemplo, la función `agregar` inserta un valor en el árbol, la función `crearArbol` crea un árbol a partir de una lista de valores, y la función `recorrerPreorden` recorre el árbol en orden preorden.

* **Función principal:**

```haskell
main :: IO ()
```

La función principal lee los datos de entrada, crea el árbol binario de búsqueda, lo recorre en diferentes órdenes, busca un valor en el árbol, elimina un valor del árbol y comprueba si el árbol está balanceado.

El código anterior es un ejemplo complejo de un árbol binario de búsqueda en Haskell. El código está bien documentado y es fácil de seguir.