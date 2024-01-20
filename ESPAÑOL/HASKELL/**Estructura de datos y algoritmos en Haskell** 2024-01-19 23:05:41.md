```haskell
-- Definición de tipos
data Lista a = Vacío | Cons a (Lista a) deriving (Show)
data Árbol a = Hoja a | Rama (Árbol a) a (Árbol a) deriving (Show)
data Pila a = Vacía | Apilar a (Pila a) deriving (Show)

-- Funciones para listas
longitud :: Lista a -> Int
longitud Vacío = 0
longitud (Cons _ xs) = 1 + longitud xs

invertir :: Lista a -> Lista a
invertir Vacío = Vacío
invertir (Cons x xs) = invertir xs `Cons` x

concatenar :: Lista a -> Lista a -> Lista a
concatenar xs ys = foldr Cons ys xs

-- Funciones para árboles
altura :: Árbol a -> Int
altura (Hoja _) = 1
altura (Rama izq _ der) = 1 + max (altura izq) (altura der)

recorrerPreorden :: Árbol a -> [a]
recorrerPreorden (Hoja x) = [x]
recorrerPreorden (Rama izq x der) = x : recorrerPreorden izq ++ recorrerPreorden der

recorrerInorden :: Árbol a -> [a]
recorrerInorden (Hoja x) = [x]
recorrerInorden (Rama izq x der) = recorrerInorden izq ++ [x] ++ recorrerInorden der

recorrerPostorden :: Árbol a -> [a]
recorrerPostorden (Hoja x) = [x]
recorrerPostorden (Rama izq x der) = recorrerPostorden izq ++ recorrerPostorden der ++ [x]

-- Funciones para pilas
apilar :: a -> Pila a -> Pila a
apilar x pila = Apilar x pila

desapilar :: Pila a -> (a, Pila a)
desapilar Vacía = error "La pila está vacía"
desapilar (Apilar x xs) = (x, xs)

-- Ejemplo de uso
listaEjemplo = Cons 1 (Cons 2 (Cons 3 Vacío))
arbolEjemplo = Rama (Hoja 1) 2 (Hoja 3)
pilaEjemplo = Apilar 1 (Apilar 2 (Apilar 3 Vacía))

-- Impresión de los resultados
print "Longitud de la lista: " ++ show (longitud listaEjemplo)
print "Lista invertida: " ++ show (invertir listaEjemplo)
print "Lista concatenada: " ++ show (concatenar listaEjemplo listaEjemplo)
print "Altura del árbol: " ++ show (altura arbolEjemplo)
print "Recorrido en preorden del árbol: " ++ show (recorrerPreorden arbolEjemplo)
print "Recorrido en inorden del árbol: " ++ show (recorrerInorden arbolEjemplo)
print "Recorrido en postorden del árbol: " ++ show (recorrerPostorden arbolEjemplo)
print "Pila con un elemento apilado: " ++ show (apilar 4 pilaEjemplo)
print "Desapilado de la pila: " ++ show (desapilar pilaEjemplo)
```

Explicación:

- Definición de tipos:

  - `Lista a`: Representa una lista de elementos de tipo `a`. Puede ser vacía (Vacío) o una secuencia de elementos (Cons).
  - `Árbol a`: Representa un árbol binario de elementos de tipo `a`. Puede ser una hoja (Hoja) o una rama con un hijo izquierdo, un valor y un hijo derecho (Rama).
  - `Pila a`: Representa una pila de elementos de tipo `a`. Puede ser vacía (Vacía) o una secuencia de elementos apilados (Apilar).

- Funciones para listas:

  - `longitud`: Devuelve la longitud de una lista.
  - `invertir`: Invierte el orden de los elementos de una lista.
  - `concatenar`: Concatena dos listas.

- Funciones para árboles:

  - `altura`: Devuelve la altura de un árbol binario (el número de niveles en el árbol).
  - `recorrerPreorden`, `recorrerInorden`, `recorrerPostorden`: Recorren un árbol binario en orden preorden, inorden y postorden, respectivamente.

- Funciones para pilas:

  - `apilar`: Apila un elemento en la parte superior de una pila.
  - `desapilar`: Desapila el elemento superior de una pila.

- Ejemplo de uso:

  - Se crean instancias de los tipos `Lista`, `Árbol` y `Pila` y se imprimen en la pantalla.