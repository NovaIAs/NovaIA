```haskell
-- Importando las bibliotecas necesarias
import Data.List (sort)
import Control.Monad (join)
import Data.Monoid (Sum(..))

-- Definiendo el tipo de dato para representar un árbol
data Arbol a = Hoja a | Rama (Arbol a) a (Arbol a)

-- Función para crear un árbol desde una lista
crearArbol :: [a] -> Arbol a
crearArbol [] = Hoja 0
crearArbol (x:xs) = Rama (crearArbol xs) x (crearArbol (tail xs))

-- Función para calcular la suma de los elementos de un árbol
sumaArbol :: Arbol Int -> Int
sumaArbol (Hoja x) = x
sumaArbol (Rama izq x der) = sumaArbol izq + x + sumaArbol der

-- Función para encontrar el elemento máximo de un árbol
maxArbol :: Arbol Int -> Int
maxArbol (Hoja x) = x
maxArbol (Rama izq x der) = max (maxArbol izq) (max (maxArbol der) x)

-- Función para encontrar el elemento mínimo de un árbol
minArbol :: Arbol Int -> Int
minArbol (Hoja x) = x
minArbol (Rama izq x der) = min (minArbol izq) (min (minArbol der) x)

-- Función para imprimir un árbol en forma de cadena
mostrarArbol :: Arbol a -> String
mostrarArbol (Hoja x) = show x
mostrarArbol (Rama izq x der) = "(" ++ mostrarArbol izq ++ "," ++ show x ++ "," ++ mostrarArbol der ++ ")"

-- Función para ordenar los elementos de un árbol
ordenarArbol :: Arbol a -> Arbol a
ordenarArbol (Hoja x) = Hoja x
ordenarArbol (Rama izq x der) = Rama (ordenarArbol izq) x (ordenarArbol der)

-- Función para crear un árbol a partir de una lista ordenada
crearArbolOrdenado :: [a] -> Arbol a
crearArbolOrdenado [] = Hoja 0
crearArbolOrdenado (x:xs) = crearArbolOrdenado xs `Rama` x `Rama` crearArbolOrdenado (tail xs)

-- Función para buscar un elemento en un árbol
buscarElemento :: Eq a => a -> Arbol a -> Bool
buscarElemento x (Hoja y) = x == y
buscarElemento x (Rama izq y der) = buscarElemento x izq || buscarElemento x der

-- Función para eliminar un elemento de un árbol
eliminarElemento :: Eq a => a -> Arbol a -> Arbol a
eliminarElemento x (Hoja y) = Hoja 0
eliminarElemento x (Rama izq y der)
  | x == y = join (izq, der)
  | x < y  = Rama (eliminarElemento x izq) y der
  | otherwise = Rama izq y (eliminarElemento x der)

-- Función para insertar un elemento en un árbol
insertarElemento :: Ord a => a -> Arbol a -> Arbol a
insertarElemento x (Hoja y) = Hoja x
insertarElemento x (Rama izq y der)
  | x < y  = Rama (insertarElemento x izq) y der
  | otherwise = Rama izq y (insertarElemento x der)

-- Función para crear un árbol a partir de una lista de listas
crearArbolDesdeListas :: [[a]] -> Arbol a
crearArbolDesdeListas [] = Hoja 0
crearArbolDesdeListas (xs:xss) = crearArbolDesdeListas xss `Rama` foldr Sum (Hoja 0) xs

-- Función para calcular la altura de un árbol
alturaArbol :: Arbol a -> Int
alturaArbol (Hoja _) = 0
alturaArbol (Rama izq _ der) = 1 + max (alturaArbol izq) (alturaArbol der)

-- Función para calcular el número de nodos de un árbol
numeroNodos :: Arbol a -> Int
numeroNodos (Hoja _) = 1
numeroNodos (Rama izq _ der) = 1 + numeroNodos izq + numeroNodos der

-- Función para calcular el número de hojas de un árbol
numeroHojas :: Arbol a -> Int
numeroHojas (Hoja _) = 1
numeroHojas (Rama izq _ der) = numeroHojas izq + numeroHojas der

-- Función para calcular el grado de un árbol
gradoArbol :: Arbol a -> Int
gradoArbol (Hoja _) = 0
gradoArbol (Rama izq _ der) = max (gradoArbol izq) (gradoArbol der) + 1

-- Función para calcular el peso de un árbol
pesoArbol :: Arbol a -> Int
pesoArbol (Hoja _) = 1
pesoArbol (Rama izq _ der) = pesoArbol izq + pesoArbol der + 1

-- Función para calcular el camino más largo de un árbol
caminoMasLargo :: Arbol a -> Int
caminoMasLargo (Hoja _) = 0
caminoMasLargo (Rama izq _ der) = 1 + max (caminoMasLargo izq) (caminoMasLargo der)

-- Función para calcular el diámetro de un árbol
diametroArbol :: Arbol a -> Int
diametroArbol (Hoja _) = 0
diametroArbol (Rama izq _ der) = max (caminoMasLargo izq + caminoMasLargo der + 1) (max (diametroArbol izq) (diametroArbol der))