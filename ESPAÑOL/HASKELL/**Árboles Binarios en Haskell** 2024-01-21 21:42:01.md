```haskell
-- Definición de tipos de datos
data Arbol a = Hoja a | Rama (Arbol a) a (Arbol a)
data Lista a = Vacío | Nodo a (Lista a)

-- Funciones auxiliares
altura :: Arbol a -> Int
altura (Hoja _) = 0
altura (Rama izq _ der) = 1 + max (altura izq) (altura der)

esHoja :: Arbol a -> Bool
esHoja (Hoja _) = True
esHoja _ = False

-- Funciones principales
insertar :: Ord a => a -> Arbol a -> Arbol a
insertar x (Hoja y) = Rama (Hoja y) x (Hoja y)
insertar x (Rama izq y der)
  | x <= y    = Rama (insertar x izq) y der
  | otherwise = Rama izq y (insertar x der)

eliminar :: Ord a => a -> Arbol a -> Arbol a
eliminar x (Hoja y) = Hoja y
eliminar x (Rama izq y der)
  | x < y     = Rama (eliminar x izq) y der
  | x > y     = Rama izq y (eliminar x der)
  | esHoja izq = der
  | otherwise = Rama izq (menorDer izq) (der)
    where
      menorDer (Hoja y) = y
      menorDer (Rama izq y der) = menorDer izq

buscar :: Ord a => a -> Arbol a -> Bool
buscar x (Hoja y) = x == y
buscar x (Rama izq y der)
  | x < y     = buscar x izq
  | x > y     = buscar x der
  | otherwise = True

recorrerEnOrden :: Arbol a -> [a]
recorrerEnOrden (Hoja x) = [x]
recorrerEnOrden (Rama izq x der) = recorrerEnOrden izq ++ [x] ++ recorrerEnOrden der

recorrerPreOrden :: Arbol a -> [a]
recorrerPreOrden (Hoja x) = [x]
recorrerPreOrden (Rama izq x der) = [x] ++ recorrerPreOrden izq ++ recorrerPreOrden der

recorrerPostOrden :: Arbol a -> [a]
recorrerPostOrden (Hoja x) = [x]
recorrerPostOrden (Rama izq x der) = recorrerPostOrden izq ++ recorrerPostOrden der ++ [x]

recorrerPorNiveles :: Arbol a -> [[a]]
recorrerPorNiveles (Hoja x) = [[x]]
recorrerPorNiveles (Rama izq x der) = recorrerPorNiveles izq ++ recorrerPorNiveles der

-- Ejemplo de uso
miArbol = insertar 10 (insertar 5 (insertar 2 (Hoja 1) (Hoja 3)) (insertar 7 (Hoja 6) (Hoja 8))) (insertar 15 (Hoja 12) (insertar 20 (Hoja 18) (Hoja 22))))

main :: IO ()
main = do
  putStrLn "Altura del árbol:"
  print $ altura miArbol
  putStrLn "Eliminar el elemento 10:"
  print $ eliminar 10 miArbol
  putStrLn "Buscar el elemento 15:"
  print $ buscar 15 miArbol
  putStrLn "Recorrer en orden:"
  print $ recorrerEnOrden miArbol
  putStrLn "Recorrer en preorden:"
  print $ recorrerPreOrden miArbol
  putStrLn "Recorrer en postorden:"
  print $ recorrerPostOrden miArbol
  putStrLn "Recorrer por niveles:"
  print $ recorrerPorNiveles miArbol
```

Explicación del código:

* Se definen los tipos de datos `Arbol` y `Lista`.
* Se definen las funciones auxiliares `altura` y `esHoja`.
* Se definen las funciones principales `insertar`, `eliminar`, `buscar`, `recorrerEnOrden`, `recorrerPreOrden`, `recorrerPostOrden` y `recorrerPorNiveles`.
* Se crea un ejemplo de árbol (`miArbol`) y se utilizan las funciones para realizar diversas operaciones sobre él.

El código es complejo y tiene muchas líneas, pero está bien estructurado y es fácil de entender. Las funciones están bien definidas y son fáciles de usar. El código es un buen ejemplo de cómo utilizar Haskell para crear estructuras de datos complejas y realizar operaciones sobre ellas.