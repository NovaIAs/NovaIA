```haskell
-- Definición de funciones recursivas

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Definición de listas

listaFibonacci :: Int -> [Int]
listaFibonacci n = take n [fibonacci i | i <- [0..]]

listaFactorial :: Int -> [Int]
listaFactorial n = take n [factorial i | i <- [0..]]

-- Definición de tuplas

listaTuplas :: Int -> [(Int, Int)]
listaTuplas n = zip (listaFibonacci n) (listaFactorial n)

-- Definición de la función principal

main :: IO ()
main = do
  print (listaFibonacci 10)
  print (listaFactorial 10)
  print (listaTuplas 10)
```

**Explicación del código:**

* La función `fibonacci` calcula el número de Fibonacci correspondiente a un número natural dado. Utiliza la recursividad para calcular el valor de cada número de Fibonacci.
* La función `factorial` calcula el factorial de un número natural dado. También utiliza la recursividad para calcular el valor de cada factorial.
* La función `listaFibonacci` genera una lista con los primeros `n` números de Fibonacci.
* La función `listaFactorial` genera una lista con los primeros `n` factoriales.
* La función `listaTuplas` genera una lista con las primeras `n` tuplas, donde cada tupla contiene un número de Fibonacci y su factorial correspondiente.
* La función `main` es la función principal del programa. Llama a las funciones `listaFibonacci`, `listaFactorial` y `listaTuplas` para generar las listas de números de Fibonacci, factoriales y tuplas, y luego las imprime en la consola.