```haskell
-- Importamos las bibliotecas necesarias
import Data.List
import Data.Maybe
import Control.Monad

-- Definimos el tipo de datos de una lista de enteros
data ListaEnteros = Vacía | Nodo Int ListaEnteros

-- Definimos la función para crear una lista de enteros
crearLista :: [Int] -> ListaEnteros
crearLista [] = Vacía
crearLista (x:xs) = Nodo x (crearLista xs)

-- Definimos la función para sumar los elementos de una lista de enteros
sumarLista :: ListaEnteros -> Int
sumarLista Vacía = 0
sumarLista (Nodo x xs) = x + sumarLista xs

-- Definimos la función para encontrar el máximo elemento de una lista de enteros
máximoLista :: ListaEnteros -> Maybe Int
máximoLista Vacía = Nothing
máximoLista (Nodo x xs) = Just (max x (fromJust (máximoLista xs)))

-- Definimos la función para encontrar el mínimo elemento de una lista de enteros
mínimoLista :: ListaEnteros -> Maybe Int
mínimoLista Vacía = Nothing
mínimoLista (Nodo x xs) = Just (min x (fromJust (mínimoLista xs)))

-- Definimos la función para ordenar una lista de enteros
ordenarLista :: ListaEnteros -> ListaEnteros
ordenarLista Vacía = Vacía
ordenarLista (Nodo x xs) = insertar x (ordenarLista xs)

-- Definimos la función para insertar un elemento en una lista de enteros ordenada
insertar :: Int -> ListaEnteros -> ListaEnteros
insertar x Vacía = Nodo x Vacía
insertar x (Nodo y ys)
  | x <= y = Nodo x (insertar x ys)
  | otherwise = Nodo y (insertar x ys)

-- Definimos la función para imprimir una lista de enteros
imprimirLista :: ListaEnteros -> String
imprimirLista Vacía = "[]"
imprimirLista (Nodo x xs) = "[" ++ show x ++ ", " ++ imprimirLista xs ++ "]"

-- Definimos la función principal
main :: IO ()
main = do
  -- Creamos una lista de enteros
  let lista = crearLista [1, 2, 3, 4, 5]

  -- Imprimimos la lista de enteros
  putStrLn (imprimirLista lista)

  -- Sumamos los elementos de la lista de enteros
  let suma = sumarLista lista

  -- Imprimimos la suma de los elementos de la lista de enteros
  putStrLn ("Suma: " ++ show suma)

  -- Encontramos el máximo elemento de la lista de enteros
  let maximo = máximoLista lista

  -- Imprimimos el máximo elemento de la lista de enteros
  putStrLn ("Máximo: " ++ show maximo)

  -- Encontramos el mínimo elemento de la lista de enteros
  let mínimo = mínimoLista lista

  -- Imprimimos el mínimo elemento de la lista de enteros
  putStrLn ("Mínimo: " ++ show mínimo)

  -- Ordenamos la lista de enteros
  let listaOrdenada = ordenarLista lista

  -- Imprimimos la lista de enteros ordenada
  putStrLn (imprimirLista listaOrdenada)
```

Este código crea una lista de enteros, suma los elementos de la lista, encuentra el máximo y el mínimo elemento de la lista, ordena la lista y la imprime. El código está comentado en español para que sea más fácil de entender.