```haskell
-- Definición de tipos de datos

data Arbol a = Hoja a
            | Nodo (Arbol a) a (Arbol a)
            deriving (Show, Eq)

-- Funciones para trabajar con árboles

-- Crear un árbol hoja
crearHoja :: a -> Arbol a
crearHoja x = Hoja x

-- Crear un árbol nodo
crearNodo :: Arbol a -> a -> Arbol a -> Arbol a
crearNodo izq x der = Nodo izq x der

-- Obtener el valor de un árbol hoja
obtenerValorHoja :: Arbol a -> a
obtenerValorHoja (Hoja x) = x

-- Obtener el valor de un árbol nodo
obtenerValorNodo :: Arbol a -> a
obtenerValorNodo (Nodo _ x _) = x

-- Obtener el árbol izquierdo de un árbol nodo
obtenerArbolIzquierdo :: Arbol a -> Arbol a
obtenerArbolIzquierdo (Nodo izq _ _) = izq

-- Obtener el árbol derecho de un árbol nodo
obtenerArbolDerecho :: Arbol a -> Arbol a
obtenerArbolDerecho (Nodo _ _ der) = der

-- Comprobar si un árbol es una hoja
esHoja :: Arbol a -> Bool
esHoja (Hoja _) = True
esHoja _ = False

-- Comprobar si un árbol es un nodo
esNodo :: Arbol a -> Bool
esNodo (Nodo _ _ _) = True
esNodo _ = False

-- Recorrer un árbol en preorden
recorrerPreorden :: Arbol a -> [a]
recorrerPreorden (Hoja x) = [x]
recorrerPreorden (Nodo izq x der) = x : recorrerPreorden izq ++ recorrerPreorden der

-- Recorrer un árbol en inorden
recorrerInorden :: Arbol a -> [a]
recorrerInorden (Hoja x) = [x]
recorrerInorden (Nodo izq x der) = recorrerInorden izq ++ [x] ++ recorrerInorden der

-- Recorrer un árbol en postorden
recorrerPostorden :: Arbol a -> [a]
recorrerPostorden (Hoja x) = [x]
recorrerPostorden (Nodo izq x der) = recorrerPostorden izq ++ recorrerPostorden der ++ [x]

-- Buscar un valor en un árbol
buscarValor :: a -> Arbol a -> Bool
buscarValor _ (Hoja _) = False
buscarValor x (Nodo izq _ der) = x == obtenerValorNodo izq || x == obtenerValorNodo der || buscarValor x izq || buscarValor x der

-- Insertar un valor en un árbol
insertarValor :: a -> Arbol a -> Arbol a
insertarValor x (Hoja _) = crearHoja x
insertarValor x (Nodo izq _ der)
  | x < obtenerValorNodo izq = crearNodo (insertarValor x izq) (obtenerValorNodo izq) der
  | x > obtenerValorNodo der = crearNodo izq (obtenerValorNodo der) (insertarValor x der)
  | otherwise = (Nodo izq x der)

-- Eliminar un valor de un árbol
eliminarValor :: a -> Arbol a -> Arbol a
eliminarValor _ (Hoja _) = crearHoja _
eliminarValor x (Nodo izq _ der)
  | x < obtenerValorNodo izq = crearNodo (eliminarValor x izq) (obtenerValorNodo izq) der
  | x > obtenerValorNodo der = crearNodo izq (obtenerValorNodo der) (eliminarValor x der)
  | otherwise = fusionarArboles izq der

-- Fusionar dos árboles
fusionarArboles :: Arbol a -> Arbol a -> Arbol a
fusionarArboles (Hoja _) (Hoja _) = crearHoja _
fusionarArboles (Hoja _) (Nodo izq x der) = crearNodo (fusionarArboles (Hoja _) izq) x der
fusionarArboles (Nodo izq x der) (Hoja _) = crearNodo izq x (fusionarArboles der (Hoja _))
fusionarArboles (Nodo izq1 x1 der1) (Nodo izq2 x2 der2)
  | x1 < x2 = crearNodo (fusionarArboles izq1 izq2) x1 (fusionarArboles der1 (Nodo izq2 x2 der2))
  | otherwise = crearNodo (fusionarArboles (Nodo izq1 x1 der1) izq2) x2 der2

-- Ejemplo de árbol
arbolEjemplo :: Arbol Int
arbolEjemplo = crearNodo
  (crearNodo
    (crearHoja 1)
    2
    (crearHoja 3))
  4
  (crearNodo
    (crearHoja 5)
    6
    (crearHoja 7))

-- Ejemplo de uso
main :: IO ()
main = do
  putStrLn "Árbol ejemplo:"
  print arbolEjemplo
  putStrLn "Recorrido en preorden:"
  print (recorrerPreorden arbolEjemplo)
  putStrLn "Recorrido en inorden:"
  print (recorrerInorden arbolEjemplo)
  putStrLn "Recorrido en postorden:"
  print (recorrerPostorden arbolEjemplo)
  putStrLn "Buscar el valor 5:"
  print (buscarValor 5 arbolEjemplo)
  putStrLn "Insertar el valor 8:"
  print (insertarValor 8 arbolEjemplo)
  putStrLn "Eliminar el valor 2:"
  print (eliminarValor 2 arbolEjemplo)
  putStrLn "Fusionar dos árboles:"
  print (fusionarArboles arbolEjemplo arbolEjemplo)
```

**Explicación del código:**

El código define un tipo de datos `Arbol a` que representa un árbol binario con valores de tipo `a`. Un árbol binario tiene tres posibles estructuras:

* Hoja: un árbol hoja es un árbol que no tiene hijos.
* Nodo: un árbol nodo es un árbol que tiene dos hijos, un hijo izquierdo y un hijo derecho.
* Vacío: un árbol vacío es un árbol que no tiene ningún hijo.

El código también define varias funciones para trabajar con árboles, incluyendo funciones para crear árboles hoja y nodo, obtener el valor de un árbol, obtener el árbol izquierdo o derecho de un árbol nodo, comprobar si un árbol es una hoja o un nodo, recorrer un árbol en preorden, inorden o postorden, buscar un valor en un árbol, insertar un valor en un árbol, eliminar un valor de un árbol y fusionar dos árboles.

El código también incluye un ejemplo de árbol binario y un ejemplo de uso de las funciones definidas para trabajar con árboles.

Este código es complejo y difícil de repetir porque combina varios conceptos de programación funcional, incluyendo tipos de datos recursivos, funciones recursivas y manejo de errores.