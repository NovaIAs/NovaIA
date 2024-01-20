```haskell
-- Definición de tipos

data Lista = Vacía | Cons Cabeza Cola

data Árbol = Hoja Valor | Rama Izquierda Derecho

-- Funciones auxiliares

longitud :: Lista -> Int
longitud Vacía = 0
longitud (Cons _ xs) = 1 + longitud xs

suma :: [Int] -> Int
suma [] = 0
suma (x:xs) = x + suma xs

producto :: [Int] -> Int
producto [] = 1
producto (x:xs) = x * producto xs

-- Funciones principales

invertir :: Lista -> Lista
invertir Vacía = Vacía
invertir (Cons cabeza cola) = invertir cola ++ [cabeza]

espejo :: Árbol -> Árbol
espejo (Hoja valor) = Hoja valor
espejo (Rama izquierda derecha) = Rama (espejo derecha) (espejo izquierda)

altura :: Árbol -> Int
altura (Hoja _) = 0
altura (Rama izquierda derecha) = 1 + max (altura izquierda) (altura derecha)

 anchura :: Árbol -> Int
anchura (Hoja _) = 1
anchura (Rama izquierda derecha) = max (anchura izquierda) (anchura derecha)

nodosHoja :: Árbol -> Int
nodosHoja (Hoja _) = 1
nodosHoja (Rama izquierda derecha) = nodosHoja izquierda + nodosHoja derecha

nodosInternos :: Árbol -> Int
nodosInternos (Hoja _) = 0
nodosInternos (Rama izquierda derecha) = 1 + nodosInternos izquierda + nodosInternos derecha

sumaValores :: Árbol -> Int
sumaValores (Hoja valor) = valor
sumaValores (Rama izquierda derecha) = sumaValores izquierda + sumaValores derecha

promedioValores :: Árbol -> Double
promedioValores árbol = sumaValores árbol / fromIntegral (nodosHoja árbol)

-- Función principal

main :: IO ()
main = do
  let lista1 = Cons 1 (Cons 2 (Cons 3 Vacía))
  let lista2 = invertir lista1
  let arbol1 = Rama (Hoja 1) (Rama (Hoja 2) (Hoja 3))
  let arbol2 = espejo arbol1
  print $ longitud lista1
  print $ longitud lista2
  print $ suma [1, 2, 3, 4, 5]
  print $ producto [1, 2, 3, 4, 5]
  print $ altura arbol1
  print $ anchura arbol1
  print $ nodosHoja arbol1
  print $ nodosInternos arbol1
  print $ sumaValores arbol1
  print $ promedioValores arbol1
```

Explicación del código:

* El código define varios tipos de datos, como `Lista` y `Árbol`.
* También define varias funciones auxiliares, como `longitud`, `suma` y `producto`.
* Las funciones principales incluyen `invertir`, `espejo`, `altura`, `anchura`, `nodosHoja`, `nodosInternos`, `sumaValores` y `promedioValores`.
* La función principal `main` crea dos listas y dos árboles, y luego llama a las funciones principales para mostrar los resultados.

Este código es complejo porque utiliza varios tipos de datos y funciones. También es diferenciado porque no es fácil de repetir.