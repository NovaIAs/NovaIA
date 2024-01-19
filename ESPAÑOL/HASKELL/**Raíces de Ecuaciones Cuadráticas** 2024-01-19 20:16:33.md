```haskell
-- Definimos un módulo llamado "EcuacionesCuadráticas".
module EcuacionesCuadráticas where

-- Definimos una función para calcular las raíces de una ecuación cuadrática.
raices :: (Double, Double, Double) -> (Double, Double)
raices (a, b, c) =
  let
    -- Calculamos el discriminante de la ecuación.
    d = b^2 - 4 * a * c
    -- Si el discriminante es negativo, no hay raíces reales.
    if d < 0 then error "No hay raíces reales."
    -- Si el discriminante es cero, hay una raíz real.
    else if d == 0 then (-b / (2 * a), -b / (2 * a))
    -- Si el discriminante es positivo, hay dos raíces reales.
    else
      let
        -- Calculamos las raíces.
        r1 = (-b + sqrt d) / (2 * a)
        r2 = (-b - sqrt d) / (2 * a)
      in
        (r1, r2)

-- Definimos una función para probar la función "raices".
prueba :: IO ()
prueba = do
  -- Pedimos al usuario que introduzca los coeficientes de la ecuación.
  putStrLn "Introduzca los coeficientes de la ecuación cuadrática (a, b, c):"
  input <- getLine
  -- Parseamos los coeficientes.
  let (a, b, c) = (read a1 :: Double, read a2 :: Double, read a3 :: Double)
  -- Calculamos las raíces.
  let (r1, r2) = raíces (a, b, c)
  -- Imprimimos las raíces.
  putStrLn "Las raíces de la ecuación son:"
  putStrLn $ "r1 = " ++ show r1
  putStrLn $ "r2 = " ++ show r2

-- Main:
main :: IO ()
main = do
  prueba
```

**Explicación:**

* El código define un módulo llamado "EcuacionesCuadráticas".
* Define una función llamada "raices" que calcula las raíces de una ecuación cuadrática.
* La función "raices" toma tres parámetros: los coeficientes de la ecuación cuadrática.
* La función calcula el discriminante de la ecuación, que es un valor que determina si la ecuación tiene raíces reales.
* Si el discriminante es negativo, no hay raíces reales.
* Si el discriminante es cero, hay una raíz real.
* Si el discriminante es positivo, hay dos raíces reales.
* La función "raices" calcula las raíces y las devuelve en una tupla.
* El código define una función llamada "prueba" que prueba la función "raices".
* La función "prueba" pide al usuario que introduzca los coeficientes de la ecuación cuadrática.
* Parseamos los coeficientes y los pasamos a la función "raices".
* La función "raices" calcula las raíces y las devuelve en una tupla.
* La función "prueba" imprime las raíces.
* El código define la función "main", que es el punto de entrada del programa.
* La función "main" llama a la función "prueba".