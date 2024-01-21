```haskell
-- Definición del tipo de datos "Lista"
data Lista a = Vacía | Nodo a (Lista a)

-- Función que añade un elemento a una lista
agregar :: a -> Lista a -> Lista a
agregar elemento lista = Nodo elemento lista

-- Función que elimina un elemento de una lista
eliminar :: Eq a => a -> Lista a -> Lista a
eliminar elemento lista = case lista of
  Vacía -> Vacía
  (Nodo cabeza resto) -> if cabeza == elemento then resto else Nodo cabeza (eliminar elemento resto)

-- Función que busca un elemento en una lista
buscar :: Eq a => a -> Lista a -> Bool
buscar elemento lista = case lista of
  Vacía -> False
  (Nodo cabeza resto) -> if cabeza == elemento then True else buscar elemento resto

-- Función que devuelve el primer elemento de una lista
primero :: Lista a -> Maybe a
primero lista = case lista of
  Vacía -> Nothing
  (Nodo cabeza _) -> Just cabeza

-- Función que devuelve el último elemento de una lista
último :: Lista a -> Maybe a
último lista = case lista of
  Vacía -> Nothing
  (Nodo _ resto) -> último resto

-- Función que devuelve la longitud de una lista
longitud :: Lista a -> Int
longitud lista = case lista of
  Vacía -> 0
  (Nodo _ resto) -> 1 + longitud resto

-- Función que concatena dos listas
concatenar :: Lista a -> Lista a -> Lista a
concatenar lista1 lista2 = case lista1 of
  Vacía -> lista2
  (Nodo cabeza resto) -> Nodo cabeza (concatenar resto lista2)

-- Función que invierte una lista
invertir :: Lista a -> Lista a
invertir lista = case lista of
  Vacía -> Vacía
  (Nodo cabeza resto) -> concatenar (invertir resto) (agregar cabeza Vacía)

-- Función que ordena una lista
ordenar :: Ord a => Lista a -> Lista a
ordenar lista = case lista of
  Vacía -> Vacía
  (Nodo cabeza resto) -> insertar cabeza (ordenar resto)

-- Función que inserta un elemento en una lista ordenada
insertar :: Ord a => a -> Lista a -> Lista a
insertar elemento lista = case lista of
  Vacía -> Nodo elemento Vacía
  (Nodo cabeza resto) -> if elemento <= cabeza then Nodo elemento lista else Nodo cabeza (insertar elemento resto)

-- Función principal
main :: IO ()
main = do
  -- Crear una lista
  lista <- return (agregar 1 (agregar 2 (agregar 3 Vacía)))

  -- Imprimir la lista
  print lista

  -- Eliminar un elemento de la lista
  listaEliminado <- return (eliminar 2 lista)

  -- Imprimir la lista eliminada
  print listaEliminado

  -- Buscar un elemento en la lista
  elementoEncontrado <- return (buscar 3 lista)

  -- Imprimir el resultado de la búsqueda
  print elementoEncontrado

  -- Obtener el primer elemento de la lista
  primerElemento <- return (primero lista)

  -- Imprimir el primer elemento
  print primerElemento

  -- Obtener el último elemento de la lista
  últimoElemento <- return (último lista)

  -- Imprimir el último elemento
  print últimoElemento

  -- Obtener la longitud de la lista
  longitudLista <- return (longitud lista)

  -- Imprimir la longitud de la lista
  print longitudLista

  -- Concatenar dos listas
  listaConcatenada <- return (concatenar lista (agregar 4 (agregar 5 Vacía))))

  -- Imprimir la lista concatenada
  print listaConcatenada

  -- Invertir una lista
  listaInvertida <- return (invertir lista)

  -- Imprimir la lista invertida
  print listaInvertida

  -- Ordenar una lista
  listaOrdenada <- return (ordenar (agregar 5 (agregar 2 (agregar 1 Vacía)))))

  -- Imprimir la lista ordenada
  print listaOrdenada
```

Explicación del código:

* Definimos el tipo de datos "Lista" utilizando la recursión, una lista puede ser vacía o un nodo que contiene un elemento y una referencia a otra lista.
* Definimos varias funciones para trabajar con listas, incluyendo agregar, eliminar, buscar, primero, último, longitud, concatenar, invertir y ordenar.
* En la función principal "main", creamos una lista, eliminamos un elemento, buscamos un elemento, obtenemos el primer y último elemento, calculamos la longitud de la lista, concatenamos dos listas, invertimos una lista y ordenamos una lista.
* Imprimimos los resultados de cada operación en la consola.

Este código es complejo y cubre una amplia gama de operaciones que se pueden realizar con listas en Haskell.