```haskell
-- LECCIÓN 1: FUNCIONES Y TIPOS DE DATOS

-- Definimos una función para calcular el factorial de un número.
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Definimos un tipo de datos para representar una lista de enteros.
data Lista = Vacía | Cons Int Lista

-- Definimos una función para sumar los elementos de una lista.
sumarLista :: Lista -> Int
sumarLista Vacía = 0
sumarLista (Cons x xs) = x + sumarLista xs

-- LECCIÓN 2: ESTRUCTURAS DE CONTROL

-- Definimos una función para comprobar si un número es par.
esPar :: Int -> Bool
esPar n = n `mod` 2 == 0

-- Definimos una función para imprimir una lista de números.
imprimirLista :: Lista -> IO ()
imprimirLista Vacía = return ()
imprimirLista (Cons x xs) = do
    putStrLn (show x)
    imprimirLista xs

-- LECCIÓN 3: MÓDULOS Y TIPOS DE DATOS PARAMÉTRICOS

-- Definimos un módulo para gestionar listas de cadenas de caracteres.
module ListasCadenas where

-- Definimos un tipo de datos para representar una lista de cadenas de caracteres.
data ListaCadenas = Vacía | Cons String ListaCadenas

-- Definimos una función para concatenar dos listas de cadenas de caracteres.
concat :: ListaCadenas -> ListaCadenas -> ListaCadenas
concat Vacía ys = ys
concat (Cons x xs) ys = Cons x (concat xs ys)

-- Definimos una función para imprimir una lista de cadenas de caracteres.
imprimirListaCadenas :: ListaCadenas -> IO ()
imprimirListaCadenas Vacía = return ()
imprimirListaCadenas (Cons x xs) = do
    putStrLn x
    imprimirListaCadenas xs

-- LECCIÓN 4: ENTRADA Y SALIDA

-- Definimos una función para leer una línea del teclado.
leerLínea :: IO String
leerLínea = getLine

-- Definimos una función para imprimir una línea en la consola.
imprimirLínea :: String -> IO ()
imprimirLínea = putStrLn

-- LECCIÓN 5: EXCEPCIONES

-- Definimos una función para dividir dos números.
dividir :: Int -> Int -> IO Int
dividir x y
  | y == 0 = error "No se puede dividir por cero"
  | otherwise = return (x `div` y)

-- LECCIÓN 6: CONCURRENCIA

-- Definimos una función para crear un hilo de ejecución.
crearHilo :: IO a -> IO ThreadId
crearHilo acción = forkIO acción

-- Definimos una función para esperar a que finalice un hilo de ejecución.
esperarHilo :: ThreadId -> IO ()
esperarHilo id = join id

-- LECCIÓN 7: GESTIÓN DE FICHEROS

-- Definimos una función para abrir un fichero.
abrirFichero :: String -> IO Handle
abrirFichero nombre = openFile nombre ReadMode

-- Definimos una función para cerrar un fichero.
cerrarFichero :: Handle -> IO ()
cerrarFichero handle = hClose handle

-- Definimos una función para leer una línea de un fichero.
leerLíneaFichero :: Handle -> IO String
leerLíneaFichero handle = hGetLine handle

-- Definimos una función para imprimir una línea en un fichero.
imprimirLíneaFichero :: Handle -> String -> IO ()
imprimirLíneaFichero handle línea = hPutStrLn handle línea
```

Explicación del código:

* **Función factorial**: Esta función calcula el factorial de un número entero. El factorial de un número es el producto de todos los enteros positivos menores o iguales que ese número. Por ejemplo, el factorial de 5 es 5 * 4 * 3 * 2 * 1 = 120.
* **Tipo de datos Lista**: Este tipo de datos representa una lista de enteros. Una lista puede ser vacía o puede estar formada por un elemento y el resto de la lista. Por ejemplo, la lista [1, 2, 3] está formada por el elemento 1 y el resto de la lista [2, 3].
* **Función sumarLista**: Esta función suma los elementos de una lista. Si la lista está vacía, la suma es 0. Si la lista no está vacía, la suma es el primer elemento de la lista más la suma del resto de la lista.
* **Función esPar**: Esta función comprueba si un número es par. Un número es par si su resto al dividirlo entre 2 es 0.
* **Función imprimirLista**: Esta función imprime una lista de números. Si la lista está vacía, no imprime nada. Si la lista no está vacía, imprime el primer elemento de la lista y luego imprime el resto de la lista.
* **Módulo ListasCadenas**: Este módulo define un tipo de datos para representar una lista de cadenas de caracteres y un conjunto de funciones para manipular listas de cadenas de caracteres.
* **Tipo de datos ListaCadenas**: Este tipo de datos representa una lista de cadenas de caracteres. Una lista de cadenas de caracteres puede ser vacía o puede estar formada por una cadena de caracteres y el resto de la lista. Por ejemplo, la lista ["Hola", "mundo", "!"] está formada por la cadena de caracteres "Hola" y el resto de la lista ["mundo", "!"].
* **Función concat**: Esta función concatena dos listas de cadenas de caracteres. Si la primera lista está vacía, devuelve la segunda lista. Si la primera lista no está vacía, devuelve una nueva lista que contiene el primer elemento de la primera lista y el resto de la lista concatenada con la segunda lista.
* **Función imprimirListaCadenas**: Esta función imprime una lista de cadenas de caracteres. Si la lista está vacía, no imprime nada. Si la lista no está vacía, imprime el primer elemento de la lista y luego imprime el resto de la lista.
* **Función leerLínea**: Esta función lee una línea del teclado.
* **Función imprimirLínea**: Esta función imprime una línea en la consola.
* **Función dividir**: Esta función divide dos números enteros. Si el segundo número es 0, devuelve un error. Si el segundo número no es 0, devuelve el resultado de dividir el primer número por el segundo número.
* **Función crearHilo**: Esta función crea un hilo de ejecución. Un hilo de ejecución es un proceso que se ejecuta simultáneamente con el programa principal.
* **Función esperarHilo**: Esta función espera a que finalice un hilo de ejecución.
* **Función abrirFichero**: Esta función abre un fichero.
* **Función cerrarFichero**: Esta función cierra un fichero.
* **Función leerLíneaFichero**: Esta función lee una línea de un fichero.
* **Función imprimirLíneaFichero**: Esta función imprime una línea en un fichero.