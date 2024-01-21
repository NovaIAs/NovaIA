```haskell
-- Definición del tipo de dato de árbol binario.
data Árbol a = Hoja a | Rama (Árbol a) a (Árbol a)

-- Función para recorrer el árbol en preorden.
recorrerPreorden :: Árbol a -> [a]
recorrerPreorden (Hoja a) = [a]
recorrerPreorden (Rama izq a der) = a : recorrerPreorden izq ++ recorrerPreorden der

-- Función para recorrer el árbol en inorden.
recorrerInorden :: Árbol a -> [a]
recorrerInorden (Hoja a) = [a]
recorrerInorden (Rama izq a der) = recorrerInorden izq ++ [a] ++ recorrerInorden der

-- Función para recorrer el árbol en postorden.
recorrerPostorden :: Árbol a -> [a]
recorrerPostorden (Hoja a) = [a]
recorrerPostorden (Rama izq a der) = recorrerPostorden izq ++ recorrerPostorden der ++ [a]

-- Función para crear un árbol binario a partir de una lista de elementos.
crearÁrbol :: [a] -> Árbol a
crearÁrbol [] = Hoja (error "La lista no puede estar vacía.")
crearÁrbol [x] = Hoja x
crearÁrbol (x:xs) = Rama (crearÁrbol xs) x (crearÁrbol (tail xs))

-- Función para insertar un elemento en un árbol binario.
insertar :: a -> Árbol a -> Árbol a
insertar x (Hoja y) = Rama (Hoja x) y (Hoja y)
insertar x (Rama izq y der)
  | x <= y    = Rama (insertar x izq) y der
  | otherwise = Rama izq y (insertar x der)

-- Función para eliminar un elemento de un árbol binario.
eliminar :: a -> Árbol a -> Árbol a
eliminar x (Hoja y) = Hoja y
eliminar x (Rama izq y der)
  | x == y    = unirÁrboles izq der
  | x < y     = Rama (eliminar x izq) y der
  | otherwise = Rama izq y (eliminar x der)
  where
    unirÁrboles (Hoja a) (Hoja b) = Hoja a
    unirÁrboles (Hoja a) (Rama izq b der) = Rama izq b der
    unirÁrboles (Rama izq a der) (Hoja b) = Rama izq a der
    unirÁrboles (Rama izq1 a der1) (Rama izq2 b der2) = Rama (unirÁrboles izq1 izq2) a (Rama der1 der2)

-- Función para buscar un elemento en un árbol binario.
buscar :: a -> Árbol a -> Bool
buscar x (Hoja y) = x == y
buscar x (Rama izq y der)
  | x == y    = True
  | x < y     = buscar x izq
  | otherwise = buscar x der

-- Función para obtener el elemento mínimo de un árbol binario.
mínimo :: Árbol a -> a
mínimo (Hoja x) = x
mínimo (Rama izq x der) = mínimo izq

-- Función para obtener el elemento máximo de un árbol binario.
máximo :: Árbol a -> a
máximo (Hoja x) = x
máximo (Rama izq x der) = máximo der

-- Función para obtener la altura de un árbol binario.
altura :: Árbol a -> Int
altura (Hoja _) = 0
altura (Rama izq _ der) = 1 + max (altura izq) (altura der)

-- Función para obtener el número de nodos de un árbol binario.
nodos :: Árbol a -> Int
nodos (Hoja _) = 1
nodos (Rama izq _ der) = 1 + nodos izq + nodos der

-- Función para obtener el número de hojas de un árbol binario.
hojas :: Árbol a -> Int
hojas (Hoja _) = 1
hojas (Rama izq _ der) = hojas izq + hojas der

-- Función para obtener el número de ramas de un árbol binario.
ramas :: Árbol a -> Int
ramas (Hoja _) = 0
ramas (Rama izq _ der) = 1 + ramas izq + ramas der

-- Función para obtener el grado de un árbol binario.
grado :: Árbol a -> Int
grado (Hoja _) = 0
grado (Rama izq _ der) = max (grado izq) (grado der) + 1

-- Función para obtener el factor de balance de un árbol binario.
balance :: Árbol a -> Int
balance (Hoja _) = 0
balance (Rama izq _ der) = altura izq - altura der

-- Función para comprobar si un árbol binario es completo.
completo :: Árbol a -> Bool
completo (Hoja _) = True
completo (Rama izq _ der) = completo izq && completo der && altura izq = altura der

-- Función para comprobar si un árbol binario es perfecto.
perfecto :: Árbol a -> Bool
perfecto (Hoja _) = True
perfecto (Rama izq _ der