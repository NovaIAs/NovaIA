```haskell
import Data.List
import Control.Monad
import Data.Char

-- Definición de los datos
data Arbol a = Hoja a | Rama (Arbol a) (Arbol a) deriving (Show, Eq)

-- Función para crear un árbol
crearArbol :: [a] -> Arbol a
crearArbol [] = Hoja ' '
crearArbol (x:xs) = Rama (crearArbol xs) (Hoja x)

-- Función para recorrer un árbol en preorden
preorden :: Arbol a -> [a]
preorden Hoja{} = []
preorden (Rama a b) = [raiz a] ++ preorden b ++ preorden a

-- Función para recorrer un árbol en inorden
inorden :: Arbol a -> [a]
inorden Hoja{} = []
inorden (Rama a b) = preorden a ++ [raiz b] ++ inorden b

-- Función para recorrer un árbol en postorden
postorden :: Arbol a -> [a]
postorden Hoja{} = []
postorden (Rama a b) = preorden a ++ preorden b ++ [raiz b]

-- Función para obtener el número de nodos de un árbol
nodoCantidad :: Arbol a -> Int
nodoCantidad Hoja{} = 1
nodoCantidad (Rama a b) = nodoCantidad a + nodoCantidad b

-- Función para obtener la altura de un árbol
alturaArbol :: Arbol a -> Int
alturaArbol Hoja{} = 0
alturaArbol (Rama a b) = max (alturaArbol a) (alturaArbol b) + 1

-- Función para determinar si un árbol es completo
esArbolCompleto :: Arbol a -> Bool
esArbolCompleto Hoja{} = True
esArbolCompleto (Rama a b) = esArbolCompleto a && esArbolCompleto b && alturaArbol (Rama a b) = 2 * alturaArbol a

-- Función para determinar si un árbol es un árbol binario de búsqueda
esArbolBinarioBusqueda :: Ord a => Arbol a -> Bool
esArbolBinarioBusqueda Hoja{} = True
esArbolBinarioBusqueda (Rama a b) = esArbolBinarioBusqueda a && esArbolBinarioBusqueda b && raiz a <= raiz b && raiz b > raiz a

-- Función para encontrar el elemento mínimo de un árbol
minimo :: Ord a => Arbol a -> a
minimo (Hoja a) = a
minimo (Rama a b) = minimo a

-- Función para encontrar el elemento máximo de un árbol
maximo :: Ord a => Arbol a -> a
maximo Hoja{} = error "El árbol está vacío"
maximo (Rama a b) = if raiz b > raiz a then maximo b else maximo a

-- Función para insertar un nuevo elemento en un árbol
insertar :: Ord a => a -> Arbol a -> Arbol a
insertar a Hoja{} = Hoja a
insertar a (Rama l r)
  | a <= raiz l = Rama (insertar a l) r
  | otherwise = Rama l (insertar a r)

-- Función para eliminar un elemento de un árbol
eliminar :: Ord a => a -> Arbol a -> Arbol a
eliminar a Hoja{} = error "El elemento no está en el árbol"
eliminar a (Rama l r)
  | a == raiz l = r
  | a == raiz r = l
  | a < raiz l = Rama (eliminar a l) r
  | otherwise = Rama l (eliminar a r)

-- Función para buscar un elemento en un árbol
buscar :: Ord a => a -> Arbol a -> Maybe a
buscar a Hoja{} = Nothing
buscar a (Rama l r)
  | a == raiz l = Just a
  | a == raiz r = Just a
  | a < raiz l = buscar a l
  | otherwise = buscar a r

-- Función para imprimir un árbol
imprimirArbol :: Arbol a -> IO ()
imprimirArbol Hoja{} = putStrLn "Hoja"
imprimirArbol (Rama l r) =
  do
    putStrLn "Rama"
    imprimirArbol l
    imprimirArbol r
```