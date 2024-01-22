```haskell
-- Definimos un tipo de dato algebraico para representar las expresiones aritméticas.

data Expresion =
    Numero Int
  | Suma Expresion Expresion
  | Resta Expresion Expresion
  | Multiplicacion Expresion Expresion
  | Division Expresion Expresion

-- Definimos una función para evaluar una expresión aritmética.

evaluar :: Expresion -> Int
evaluar (Numero n) = n
evaluar (Suma e1 e2) = evaluar e1 + evaluar e2
evaluar (Resta e1 e2) = evaluar e1 - evaluar e2
evaluar (Multiplicacion e1 e2) = evaluar e1 * evaluar e2
evaluar (Division e1 e2) = evaluar e1 `div` evaluar e2

-- Definimos una función para imprimir una expresión aritmética.

imprimir :: Expresion -> String
imprimir (Numero n) = show n
imprimir (Suma e1 e2) = imprimir e1 ++ " + " ++ imprimir e2
imprimir (Resta e1 e2) = imprimir e1 ++ " - " ++ imprimir e2
imprimir (Multiplicacion e1 e2) = imprimir e1 ++ " * " ++ imprimir e2
imprimir (Division e1 e2) = imprimir e1 ++ " / " ++ imprimir e2

-- Definimos una función para simplificar una expresión aritmética.

simplificar :: Expresion -> Expresion
simplificar (Suma (Numero 0) e) = simplificar e
simplificar (Suma e (Numero 0)) = simplificar e
simplificar (Resta (Numero 0) e) = simplificar (Numero (-1)) * simplificar e
simplificar (Resta e (Numero 0)) = simplificar e
simplificar (Multiplicacion (Numero 0) _) = Numero 0
simplificar (Multiplicacion _ (Numero 0)) = Numero 0
simplificar (Multiplicacion (Numero 1) e) = simplificar e
simplificar (Multiplicacion e (Numero 1)) = simplificar e
simplificar (Division (Numero 0) _) = Numero 0
simplificar (Division e (Numero 1)) = simplificar e
simplificar (Suma e1 e2) = Suma (simplificar e1) (simplificar e2)
simplificar (Resta e1 e2) = Resta (simplificar e1) (simplificar e2)
simplificar (Multiplicacion e1 e2) = Multiplicacion (simplificar e1) (simplificar e2)
simplificar (Division e1 e2) = Division (simplificar e1) (simplificar e2)

-- Definimos una función para factorizar una expresión aritmética.

factorizar :: Expresion -> Expresion
factorizar (Suma e1 e2) = Suma (factorizar e1) (factorizar e2)
factorizar (Resta e1 e2) = Resta (factorizar e1) (factorizar e2)
factorizar (Multiplicacion e1 e2) =
    if esPrimo (evaluar e1) && esPrimo (evaluar e2)
    then Multiplicacion e1 e2
    else factorizar e1 * factorizar e2
factorizar (Division e1 e2) = Division (factorizar e1) (factorizar e2)

-- Definimos una función para determinar si un número es primo.

esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n
    | n `mod` 2 == 0 = False
    | otherwise = todosLosFactores n `elem` [1, n]
  where
    todosLosFactores :: Int -> [Int]
    todosLosFactores n = [2 .. n] `filter` (\k -> n `mod` k == 0)

-- Definimos una función para encontrar todos los factores de un número.

todosLosFactores :: Int -> [Int]
todosLosFactores n = [2 .. n] `filter` (\k -> n `mod` k == 0)

-- Imprimimos el resultado de evaluar, simplificar, factorizar y encontrar todos los
-- factores de algunas expresiones aritméticas.

main :: IO ()
main = do
    putStrLn "Evaluación:"
    print $ evaluar (Suma (Numero 1) (Multiplicacion (Numero 2) (Numero 3)))
    print $ evaluar (Resta (Numero 5) (Division (Numero 10) (Numero 2)))

    putStrLn "Simplificación:"
    print $ simplificar (Suma (Numero 1) (Suma (Numero 2) (Numero 3)))
    print $ simplificar (Resta (Numero 5) (Resta (Numero 10) (Numero 2)))

    putStrLn "Factorización:"
    print $ factorizar (Multiplicacion (Numero 12) (Numero 15))
    print $ factorizar (Division (Numero 25) (Numero 5))

    putStrLn "Factores:"
    print $ todosLosFactores 10
    print $ todosLosFactores 12
```

Explicación del código:

* Definimos un tipo de dato algebraico llamado `Expresion` para representar las expresiones aritméticas. Este tipo de dato tiene cinco constructores: `Numero`, `Suma`, `Resta`, `Multiplicacion` y `Division`.
* Definimos una función llamada `evaluar` que evalúa una expresión aritmética y devuelve su valor.
* Definimos una función llamada `imprimir` que imprime una expresión aritmética en forma de cadena de caracteres.
* Definimos una función llamada `simplificar` que simplifica una expresión aritmética eliminando los términos innecesarios.
* Definimos una función llamada `factorizar` que factoriza una expresión aritmética en sus factores primos.
* Definimos una función llamada `esPrimo` que determina si un número es primo.
* Definimos una función llamada `todosLosFactores` que encuentra todos los factores de un número.
* Imprimimos el resultado de evaluar, simplificar, factorizar y encontrar todos los factores de algunas expresiones aritméticas.