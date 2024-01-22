```haskell
import Data.List (scanl, groupBy, sort)

-- Representación de un polinomio como una lista de coeficientes,
-- con el coeficiente del término de mayor grado al principio.
type Polinomio = [Int]

-- Función auxiliar para evaluar un polinomio en un punto dado.
evaluar :: Polinomio -> Double -> Double
evaluar (a:as) x = a + x * evaluar as x

-- Función para calcular las raíces de un polinomio mediante el método de Newton-Raphson.
raices :: Polinomio -> [Double]
raices p = newtonRaphson p (replicate (length p - 1) 0)

-- Función auxiliar para aplicar el método de Newton-Raphson.
newtonRaphson :: Polinomio -> [Double] -> [Double]
newtonRaphson p xs =
  let ys = zipWith evaluar p xs
      zs = zipWith3 (\x y z -> x - y / z) xs ys ys
   in if all (\z -> abs z < 1e-6) zs
      then xs
      else newtonRaphson p zs

-- Función para calcular la derivada de un polinomio.
derivada :: Polinomio -> Polinomio
derivada (a:as) = zipWith (*) [1..(length as)] as

-- Función para determinar si un polinomio tiene raíces reales.
tieneRaicesReales :: Polinomio -> Bool
tieneRaicesReales p = any (< 0) (scanl (*) 1 (derivada p))

-- Función para calcular el polinomio característico de una matriz.
polinomioCaracteristico :: [[Double]] -> Polinomio
polinomioCaracteristico m =
  let n = length m
   in scanl (\acc i -> acc - (m !! i !! i) * (polinomioCaracteristico' (removerFilaYColumna i m))) 0 [1..n]
  where
    removerFilaYColumna i m =
      take i m ++ drop (i + 1) m
      [filter (\x -> x /= m !! i !! i) $ take i (m !! i)]
      ++ drop (i + 1) (m !! i)
      ++ take i (drop (i + 1) m)

-- Función auxiliar para calcular el polinomio característico'.
polinomioCaracteristico' :: [[Double]] -> Polinomio
polinomioCaracteristico' m =
  let n = length m
   in if n == 1
      then [m !! 0 !! 0]
      else map negate (diagonales (polinomioCaracteristico' (removerFilaYColumna 0 m)))

-- Función auxiliar para calcular las diagonales de una matriz.
diagonales :: [[Double]] -> [Double]
diagonales m =
  let n = length m
   in scanl (\acc i -> acc ++ [m !! i !! i]) [] [0..n-1]

-- Función para calcular los valores propios de una matriz.
valoresPropios :: [[Double]] -> [Double]
valoresPropios m = map evaluar (raices (polinomioCaracteristico m))

-- Función para calcular los vectores propios de una matriz.
vectoresPropios :: [[Double]] -> [[Double]]
vectoresPropios m =
  let n = length m
      valores = valoresPropios m
   in [calcularVectorPropio m valor | valor <- valores]

-- Función auxiliar para calcular un vector propio asociado a un valor propio.
calcularVectorPropio :: [[Double]] -> Double -> [Double]
calcularVectorPropio m valor =
  let aux = zipWith3 (\x y z -> x - y * z) [1..(length m)] (map evaluar m) (replicate (length m - 1) 0)
   in normalizar (resolverSistema aux (replicate (length m - 1) 0) valor)

-- Función auxiliar para resolver un sistema de ecuaciones lineales.
resolverSistema :: [[Double]] -> [Double] -> Double -> [Double]
resolverSistema m xs valor =
  let n = length m
   in calcularCoeficientes (escalonar m xs) valor
  where
    escalonar :: [[Double]] -> [Double] -> [[Double]]
    escalonar m xs = foldl escalonarFila m xs
      where
        escalonarFila :: [[Double]] -> [Double] -> [[Double]]
        escalonarFila m xs =
          let i = head (filter (\i -> m !! i !! 0 /= 0) [0..n-1])
              fila = m !! i
              pivote = fila !! 0
           in zipWith (\f x -> (f - (x / pivote) * fila)) m $ drop 1 xs

    calcularCoeficientes :: [[Double]] -> Double -> [Double]
    calcularCoeficientes m valor = go 0
      where
        go i =
          if i == n
          then xs
          else case m !! i !! i of
            0.0 -> error "Sistema incompatible o indeterminado"
            _ ->
              let x = (valor - sum (zipWith (*) (drop (i + 1) (m !! i)) (drop (i + 1) xs))) / m !! i !! i
               in x : go (i + 1)

-- Función auxiliar para normalizar un vector.
normalizar :: [Double] -> [Double]
normalizar xs = map (/ sqrt (sum (map (^ 2) xs))) xs

-- Función para diagonalizar una matriz.
diagonalizar :: [[Double]] -> ([[Double]], [[Double]])
diagonalizar m =
  let valores = valoresPropios m
      vectores = vectoresPropios m
   in (valores, vectores)

-- Función para calcular la matriz exponencial de una matriz.
exponencial :: [[Double]] -> Double -> [[Double]]
exponencial m t =
  let (valores, vectores) = diagonalizar m
      matrizDiagonal =
        map (\(valor, vector) ->
          vector ++ [exp (valor * t)])
          (zip valores vectores)
   in vectores ** matrizDiagonal ** (transpose vectores)

-- Función para calcular la matriz logarítmica de una matriz.
logaritmo :: [[Double]] -> Double -> [[Double]]
logaritmo m t =
  let (valores, vectores) = diagonalizar m
      matrizDiagonal =
        map (\(valor, vector) ->
          vector ++ [log (valor * t)])
          (zip valores vectores)
   in vectores ** matrizDiagonal ** (transpose vectores)

-- Función para elevar una matriz a una potencia entera.
potencia :: [[Double]] -> Int -> [[Double]]
potencia m n =
  if n == 0
  then identidad
  else if n == 1
  then m
  else if even n
  then potencia (m ** 2) (n `div` 2)
  else m ** potencia (m ** 2) (n `div` 2)

-- Función para calcular la identidad de una matriz.
identidad :: Int -> [[Double]]
identidad n = [replicate n 0 | _ <- [1..n]]

-- Función para calcular el producto de dos matrices.
(**) :: [[Double]] -> [[Double]] -> [[Double]]
(**) m1 m2 = zipWith (map sum) m1 (transpose m2)

-- Función para calcular la transpuesta de una matriz.
transpose :: [[Double]] -> [[Double]]
transpose m = [map (\x -> m !! i !! x) [0..(length m - 1)] | i <- [0..(length m - 1)]]

-- Función para calcular la inversa de una matriz.
inversa :: [[Double]] -> [[Double]]
inversa m = foldl escalonarFila identidad m
  where
    escalonarFila :: [[Double]] -> [[Double]] -> [[Double]]
    escalonarFila m xs =
      let i = head (filter (\i -> m !! i !! 0 /= 0) [0..(length m - 1)])
          fila = m !! i
          pivote = fila !! 0
           in zipWith (\f x -> (f - (x / pivote) * filas)) m (drop 1 xs)

-- Función para calcular el determinante de una matriz.
determinante :: [[Double]] -> Double
determinante m =
  let n = length m
   in sum (map (\(i, fi