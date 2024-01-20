```haskell
-- Importar las bibliotecas necesarias
import Data.List (scanl, nub, sort)
import Data.Char (ord, chr)

-- Definir el tipo de datos Árbol
data Árbol = Hoja | Rama Árbol Árbol

-- Definir la función de inserción en un árbol
insertar :: Árbol -> Árbol -> Árbol
insertar Hoja a = Rama Hoja a
insertar (Rama izq der) a
  | a <= der = Rama izq (insertar der a)
  | otherwise = Rama (insertar izq a) der

-- Definir la función de búsqueda en un árbol
buscar :: Árbol -> Árbol -> Bool
buscar Hoja _ = False
buscar (Rama izq der) a
  | a == der = True
  | a <= der = buscar izq a
  | otherwise = buscar der a

-- Definir la función de eliminación de un árbol
eliminar :: Árbol -> Árbol -> Árbol
eliminar Hoja _ = Hoja
eliminar (Rama izq der) a
  | a == der = izq
  | a <= der = Rama izq (eliminar der a)
  | otherwise = Rama (eliminar izq a) der

-- Definir la función de recorrido en orden de un árbol
recorridoEnOrden :: Árbol -> [Árbol]
recorridoEnOrden Hoja = []
recorridoEnOrden (Rama izq der) = recorridoEnOrden izq ++ [der] ++ recorridoEnOrden der

-- Definir la función de recorrido en preorden de un árbol
recorridoEnPreorden :: Árbol -> [Árbol]
recorridoEnPreorden Hoja = []
recorridoEnPreorden (Rama izq der) = [der] ++ recorridoEnPreorden izq ++ recorridoEnPreorden der

-- Definir la función de recorrido en postorden de un árbol
recorridoEnPostorden :: Árbol -> [Árbol]
recorridoEnPostorden Hoja = []
recorridoEnPostorden (Rama izq der) = recorridoEnPostorden izq ++ recorridoEnPostorden der ++ [der]

-- Definir la función de altura de un árbol
altura :: Árbol -> Int
altura Hoja = 0
altura (Rama izq der) = 1 + max (altura izq) (altura der)

-- Definir la función de número de nodos de un árbol
numeroDeNodos :: Árbol -> Int
numeroDeNodos Hoja = 0
numeroDeNodos (Rama izq der) = 1 + numeroDeNodos izq + numeroDeNodos der

-- Definir la función de número de hojas de un árbol
numeroDeHojas :: Árbol -> Int
numeroDeHojas Hoja = 1
numeroDeHojas (Rama izq der) = numeroDeHojas izq + numeroDeHojas der

-- Definir la función de ancho de un árbol
ancho :: Árbol -> Int
ancho Hoja = 1
ancho (Rama izq der) = max (ancho izq) (ancho der) + 1

-- Definir la función de profundidad de un árbol
profundidad :: Árbol -> Int
profundidad Hoja = 0
profundidad (Rama izq der) = max (profundidad izq) (profundidad der) + 1

-- Definir la función de balance de un árbol
balance :: Árbol -> Int
balance Hoja = 0
balance (Rama izq der) = altura izq - altura der

-- Definir la función de simetría de un árbol
simetria :: Árbol -> Bool
simetria Hoja = True
simetria (Rama izq der) = simetria izq && simetria der && (recorridoEnPreorden izq == recorridoEnPreorden der)

-- Definir la función de árbol binario de búsqueda
arbolBinarioDeBusqueda :: [Árbol] -> Árbol
arbolBinarioDeBusqueda [] = Hoja
arbolBinarioDeBusqueda (a:as) = foldl insertar Hoja (a:as)

-- Definir la función de árbol binario completo
arbolBinarioCompleto :: Int -> Árbol
arbolBinarioCompleto 0 = Hoja
arbolBinarioCompleto n = Rama (arbolBinarioCompleto (n-1)) (arbolBinarioCompleto (n-1))

-- Definir la función de árbol binario perfecto
arbolBinarioPerfecto :: Int -> Árbol
arbolBinarioPerfecto 0 = Hoja
arbolBinarioPerfecto n = Rama (arbolBinarioPerfecto (n-1)) (arbolBinarioPerfecto (n-1))

-- Definir la función de árbol binario degenerado
arbolBinarioDegenerado :: Int -> Árbol
arbolBinarioDegenerado 0 = Hoja
arbolBinarioDegenerado n = Rama (arbolBinarioDegenerado (n-1)) Hoja

-- Definir la función de árbol binario aleatorio
arbolBinarioAleatorio :: Int -> Árbol
arbolBinarioAleatorio 0 = Hoja
arbolBinarioAleatorio n = Rama (arbolBinarioAleatorio (n-1)) (arbolBinarioAleatorio (n-1))

-- Definir la función de árbol binario equilibrado
arbolBinarioEquilibrado :: Int -> Árbol
arbolBinarioEquilibrado 0 = Hoja
arbolBinarioEquilibrado n = Rama (arbolBinarioEquilibrado (n-1)) (arbolBinarioEquilibrado (n-1))
```

Explicación:

* El código define una serie de funciones para trabajar con árboles binarios.
* Las funciones principales son: insertar, buscar, eliminar, recorridoEnOrden, recorridoEnPreorden, recorridoEnPostorden, altura, numeroDeNodos, numeroDeHojas, ancho, profundidad, balance, simetría, arbolBinarioDeBusqueda, arbolBinarioCompleto, arbolBinarioPerfecto, arbolBinarioDegenerado y arbolBinarioAleatorio.
* Las funciones de recorrido (recorridoEnOrden, recorridoEnPreorden y recorridoEnPostorden) devuelven una lista con los elementos del árbol en el orden especificado.
* Las funciones de altura, numeroDeNodos, numeroDeHojas, ancho, profundidad y balance devuelven un valor numérico que representa la altura, el número de nodos, el número de hojas, el ancho, la profundidad y el balance del árbol, respectivamente.
* La función de simetría devuelve un valor booleano que indica si el árbol es simétrico o no.
* Las funciones de arbolBinarioDeBusqueda, arbolBinarioCompleto, arbolBinarioPerfecto, arbolBinarioDegenerado y arbolBinarioAleatorio devuelven un árbol binario a partir de una lista de elementos, un número de nodos, un número de niveles, un número de hojas o un número de elementos, respectivamente.
* La función de arbolBinarioEquilibrado devuelve un árbol binario equilibrado a partir de un número de nodos.