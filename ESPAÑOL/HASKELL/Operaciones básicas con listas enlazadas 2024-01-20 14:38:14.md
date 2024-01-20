```haskell
-- Definir un tipo de datos para representar una lista enlazada.
data ListaEnlazada a = Vacía | Nodo a (ListaEnlazada a)

-- Definir una función para crear una lista enlazada a partir de una lista de elementos.
crearLista :: [a] -> ListaEnlazada a
crearLista [] = Vacía
crearLista (x:xs) = Nodo x (crearLista xs)

-- Definir una función para obtener el primer elemento de una lista enlazada.
primero :: ListaEnlazada a -> a
primero Vacía = error "La lista está vacía."
primero (Nodo x _) = x

-- Definir una función para obtener el resto de una lista enlazada.
resto :: ListaEnlazada a -> ListaEnlazada a
resto Vacía = error "La lista está vacía."
resto (Nodo _ xs) = xs

-- Definir una función para agregar un elemento al final de una lista enlazada.
agregarFinal :: a -> ListaEnlazada a -> ListaEnlazada a
agregarFinal x Vacía = Nodo x Vacía
agregarFinal x (Nodo y ys) = Nodo y (agregarFinal x ys)

-- Definir una función para eliminar el primer elemento de una lista enlazada.
eliminarPrimero :: ListaEnlazada a -> ListaEnlazada a
eliminarPrimero Vacía = error "La lista está vacía."
eliminarPrimero (Nodo _ xs) = xs

-- Definir una función para concatenar dos listas enlazadas.
concatenar :: ListaEnlazada a -> ListaEnlazada a -> ListaEnlazada a
concatenar Vacía ys = ys
concatenar (Nodo x xs) ys = Nodo x (concatenar xs ys)

-- Definir una función para invertir una lista enlazada.
invertir :: ListaEnlazada a -> ListaEnlazada a
invertir Vacía = Vacía
invertir (Nodo x xs) = concatenar (invertir xs) (Nodo x Vacía)

-- Definir una función para ordenar una lista enlazada.
ordenar :: Ord a => ListaEnlazada a -> ListaEnlazada a
ordenar Vacía = Vacía
ordenar (Nodo x xs) = insertar (ordenar xs) x

-- Definir una función para insertar un elemento en una lista enlazada ordenada.
insertar :: Ord a => ListaEnlazada a -> a -> ListaEnlazada a
insertar Vacía x = Nodo x Vacía
insertar (Nodo y ys) x
  | x <= y = Nodo x (insertar ys x)
  | otherwise = Nodo y (insertar ys x)

-- Definir una función para buscar un elemento en una lista enlazada ordenada.
buscar :: Ord a => a -> ListaEnlazada a -> Bool
buscar x Vacía = False
buscar x (Nodo y ys)
  | x == y = True
  | x < y = buscar x ys
  | otherwise = False

-- Probar las funciones.
lista1 = crearLista [1, 2, 3, 4, 5]
lista2 = crearLista ['a', 'b', 'c', 'd', 'e']

-- Imprimir la lista enlazada.
printLista :: ListaEnlazada a -> IO ()
printLista Vacía = putStrLn "La lista está vacía."
printLista (Nodo x xs) = do
  putStrLn $ show x
  printLista xs

-- Probar la función para obtener el primer elemento de una lista enlazada.
printPrimero :: ListaEnlazada a -> IO ()
printPrimero lista = do
  let x = primero lista
  putStrLn $ "El primer elemento de la lista es: " ++ show x

-- Probar la función para obtener el resto de una lista enlazada.
printResto :: ListaEnlazada a -> IO ()
printResto lista = do
  let xs = resto lista
  putStrLn $ "El resto de la lista es: " ++ show xs

-- Probar la función para agregar un elemento al final de una lista enlazada.
printAgregarFinal :: a -> ListaEnlazada a -> IO ()
printAgregarFinal x lista = do
  let ys = agregarFinal x lista
  putStrLn $ "La lista con el elemento " ++ show x ++ " agregado al final es: " ++ show ys

-- Probar la función para eliminar el primer elemento de una lista enlazada.
printEliminarPrimero :: ListaEnlazada a -> IO ()
printEliminarPrimero lista = do
  let xs = eliminarPrimero lista
  putStrLn $ "La lista con el primer elemento eliminado es: " ++ show xs

-- Probar la función para concatenar dos listas enlazadas.
printConcatenar :: ListaEnlazada a -> ListaEnlazada a -> IO ()
printConcatenar lista1 lista2 = do
  let ys = concatenar lista1 lista2
  putStrLn $ "La lista concatenada es: " ++ show ys

-- Probar la función para invertir una lista enlazada.
printInvertir :: ListaEnlazada a -> IO ()
printInvertir lista = do
  let ys = invertir lista
  putStrLn $ "La lista invertida es: " ++ show ys

-- Probar la función para ordenar una lista enlazada.
printOrdenar :: Ord a => ListaEnlazada a -> IO ()
printOrdenar lista = do
  let ys = ordenar lista
  putStrLn $ "La lista ordenada es: " ++ show ys

-- Probar la función para insertar un elemento en una lista enlazada ordenada.
printInsertar :: Ord a => a -> ListaEnlazada a -> IO ()
printInsertar x lista = do
  let ys = insertar lista x
  putStrLn $ "La lista con el elemento " ++ show x ++ " insertado es: " ++ show ys

-- Probar la función para buscar un elemento en una lista enlazada ordenada.
printBuscar :: Ord a => a -> ListaEnlazada a -> IO ()
printBuscar x lista = do
  let encontrado = buscar x lista
  if encontrado then
    putStrLn $ "El elemento " ++ show x ++ " se encontró en la lista."
  else
    putStrLn $ "El elemento " ++ show x ++ " no se encontró en la lista."
```