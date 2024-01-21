```haskell
import Data.List (group, sort)
import Data.Maybe (fromJust, isJust)

-- 1. Definir el tipo de dato de un árbol binario de búsqueda.

data Arbol a = Hoja | Nodo a (Arbol a) (Arbol a)
  deriving (Show, Eq)

-- 2. Definir una función para crear un árbol binario de búsqueda a partir de una lista de valores.

crearArbol :: Ord a => [a] -> Arbol a
crearArbol [] = Hoja
crearArbol (x:xs) =
  let
    (menores, mayores) = partition (< x) xs
    izquierdo = crearArbol menores
    derecho = crearArbol mayores
  in Nodo x izquierdo derecho

-- 3. Definir una función para insertar un valor en un árbol binario de búsqueda.

insertar :: Ord a => a -> Arbol a -> Arbol a
insertar x Hoja = Nodo x Hoja Hoja
insertar x (Nodo y izquierdo derecho)
  | x == y = Nodo y izquierdo derecho
  | x < y = Nodo y (insertar x izquierdo) derecho
  | otherwise = Nodo y izquierdo (insertar x derecho)

-- 4. Definir una función para eliminar un valor de un árbol binario de búsqueda.

eliminar :: Ord a => a -> Arbol a -> Arbol a
eliminar _ Hoja = Hoja
eliminar x (Nodo y izquierdo derecho)
  | x == y = eliminarNodo y izquierdo derecho
  | x < y = Nodo y (eliminar x izquierdo) derecho
  | otherwise = Nodo y izquierdo (eliminar x derecho)

eliminarNodo :: Ord a => a -> Arbol a -> Arbol a -> Arbol a
eliminarNodo y Hoja Hoja = Hoja
eliminarNodo y (Nodo x izquierdo derecho) Hoja
  | x < y = Nodo x izquierdo Hoja
  | otherwise = Nodo x Hoja derecho
eliminarNodo y Hoja (Nodo x izquierdo derecho)
  | x < y = Nodo x Hoja derecho
  | otherwise = Nodo x izquierdo Hoja
eliminarNodo y (Nodo x izquierdo derecho) (Nodo z izq der)
  | x < y = Nodo x izquierdo (eliminarNodo y izq der)
  | x > y = Nodo x izquierdo derecho (eliminarNodo y izq der)
  | otherwise = desdeJusto izq z der

desdeJusto :: Ord a => Arbol a -> a -> Arbol a -> Arbol a
desdeJusto Hoja _ _ = Hoja
desdeJusto (Nodo y izquierdo derecho) x (Nodo z izq der)
  | y < x = Nodo y izquierdo (desdeJusto derecho x (Nodo z izq der))
  | y > x = Nodo y (desdeJusto izquierdo x (Nodo z izq der))
  | otherwise = desdeJusto izq z der

-- 5. Definir una función para buscar un valor en un árbol binario de búsqueda.

buscar :: Ord a => a -> Arbol a -> Maybe a
buscar _ Hoja = Nothing
buscar x (Nodo y izquierdo derecho)
  | x == y = Just y
  | x < y = buscar x izquierdo
  | otherwise = buscar x derecho

-- 6. Definir una función para obtener el valor mínimo de un árbol binario de búsqueda.

minimo :: Ord a => Arbol a -> Maybe a
minimo Hoja = Nothing
minimo (Nodo y Hoja _) = Just y
minimo (Nodo y izquierdo _) = minimo izquierdo

-- 7. Definir una función para obtener el valor máximo de un árbol binario de búsqueda.

maximo :: Ord a => Arbol a -> Maybe a
maximo Hoja = Nothing
maximo (Nodo y _ Hoja) = Just y
maximo (Nodo y _ derecho) = maximo derecho

-- 8. Definir una función para obtener la altura de un árbol binario de búsqueda.

altura :: Arbol a -> Int
altura Hoja = 0
altura (Nodo _ izquierdo derecho) = 1 + max (altura izquierdo) (altura derecho)

-- 9. Definir una función para imprimir un árbol binario de búsqueda en orden.

orden :: Arbol a -> [a]
orden Hoja = []
orden (Nodo y izquierdo derecho) = orden izquierdo ++ [y] ++ orden derecho

-- 10. Definir una función para imprimir un árbol binario de búsqueda en preorden.

preorden :: Arbol a -> [a]
preorden Hoja = []
preorden (Nodo y izquierdo derecho) = [y] ++ preorden izquierdo ++ preorden derecho

-- 11. Definir una función para imprimir un árbol binario de búsqueda en postorden.

postorden :: Arbol a -> [a]
postorden Hoja = []
postorden (Nodo y izquierdo derecho) = postorden izquierdo ++ postorden derecho ++ [y]

-- 12. Definir una función para generar una lista de todos los valores contenidos en un árbol binario de búsqueda.

lista :: Arbol a -> [a]
lista Hoja = []
lista (Nodo y izquierdo derecho) = lista izquierdo ++ [y] ++ lista derecho

-- 13. Definir una función para generar una lista de todos los valores contenidos en un árbol binario de búsqueda agrupados por nivel.

niveles :: Arbol a -> [[a]]
niveles Hoja = []
niveles (Nodo y izquierdo derecho) =
  let izquierdoNiveles = niveles izquierdo
      derechoNiveles = niveles derecho
  in zipWith (++) (y : izquierdoNiveles) derechoNiveles

-- 14. Definir una función para generar una lista de todos los valores contenidos en un árbol binario de búsqueda ordenados en orden ascendente.

ordenados :: Ord a => Arbol a -> [a]
ordenados Hoja = []
ordenados (Nodo y izquierdo derecho) = ordenados izquierdo ++ [y] ++ ordenados derecho

-- 15. Definir una función para generar una lista de todos los valores contenidos en un árbol binario de búsqueda ordenados en orden descendente.

descendentes :: Ord a => Arbol a -> [a]
descendentes Hoja = []
descendentes (Nodo y izquierdo derecho) = descendentes derecho ++ [y] ++ descendentes izquierdo

-- 16. Definir una función para verificar si un árbol binario de búsqueda es un árbol binario de búsqueda válido.

esArbolBinarioDeBusqueda :: Ord a => Arbol a -> Bool
esArbolBinarioDeBusqueda Hoja = True
esArbolBinarioDeBusqueda (Nodo y izquierdo derecho) =
  let
    esIzquierdoArbolBinarioDeBusqueda = esArbolBinarioDeBusqueda izquierdo
    esDerechoArbolBinarioDeBusqueda = esArbolBinarioDeBusqueda derecho
    esIzquierdoOrdenadoMenorQueDerecho = all (< y) (lista izquierdo)
    esDerechoOrdenadoMayorQueIzquierdo = all (> y) (lista derecho)
  in
    esIzquierdoArbolBinarioDeBusqueda
    && esDerechoArbolBinarioDeBusqueda
    && esIzquierdoOrdenadoMenorQueDerecho
    && esDerechoOrdenadoMayorQueIzquierdo

-- 17. Definir una función para verificar si un árbol binario de búsqueda es un árbol binario de búsqueda perfecto.

esArbolBinarioDeBusquedaPerfecto :: Ord a => Arbol a -> Bool
esArbolBinarioDeBusquedaPerfecto Hoja = True
esArbolBinarioDeBusquedaPerfecto (Nodo _ izquierdo derecho) =
  let
    esIzquierdoArbolBinarioDeBusquedaPerfecto = esArbolBinarioDeBusquedaPerfecto izquierdo
    esDerechoArbolBinarioDeBusquedaPerfecto = esArbolBinarioDeBusquedaPerfecto derecho
    esAlturaIzquierdaIgualAAlturaDerecha = altura izquierdo == altura derecho
  in
    esIzquierdoArbolBinarioDeBusquedaPerfecto
    && esDerechoArbolBinarioDeBusquedaPerfecto
    && esAlturaIzquierdaIgualAAlturaDerecha

-- 18. Definir una función para verificar si un árbol binario de búsqueda es un árbol binario de búsqueda completo.

esArbolBinarioDeBusquedaCompleto :: Ord a => Arbol a -> Bool
esArbolBinarioDeBusquedaCompleto Hoja = True
esArbolBinarioDeBusquedaCompleto (Nodo _ izquierdo derecho) =
  let
    esIzquierdoArbolBinarioDeBusquedaCompleto = esArbolBinarioDeBusquedaCompleto izquierdo
    esDerechoArbolBinarioDeBusquedaCompleto = esArbolBinarioDeBusquedaCompleto
```