```haskell
module Complejo where

-- Definición de tipos

-- Un número complejo es una pareja de flotantes.
type Complejo = (Float, Float)

-- Función sumar

-- La función sumar toma dos números complejos y devuelve su suma.
sumar :: Complejo -> Complejo -> Complejo
sumar (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Función restar

-- La función restar toma dos números complejos y devuelve su resta.
restar :: Complejo -> Complejo -> Complejo
restar (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

-- Función multiplicar

-- La función multiplicar toma dos números complejos y devuelve su producto.
multiplicar :: Complejo -> Complejo -> Complejo
multiplicar (x1, y1) (x2, y2) = ((x1 * x2) - (y1 * y2), (x1 * y2) + (y1 * x2))

-- Función dividir

-- La función dividir toma dos números complejos y devuelve su división.
dividir :: Complejo -> Complejo -> Complejo
dividir (x1, y1) (x2, y2) = (((x1 * x2) + (y1 * y2)) / ((x2 * x2) + (y2 * y2)), ((y1 * x2) - (x1 * y2)) / ((x2 * x2) + (y2 * y2)))

-- Función módulo

-- La función módulo toma un número complejo y devuelve su módulo.
modulo :: Complejo -> Float
modulo (x, y) = sqrt (x * x + y * y)

-- Función argumento

-- La función argumento toma un número complejo y devuelve su argumento.
argumento :: Complejo -> Float
argumento (x, y) = atan2 y x

-- Función exponencial

-- La función exponencial toma un número complejo y devuelve su exponencial.
exponencial :: Complejo -> Complejo
exponencial (x, y) = (exp x * cos y, exp x * sin y)

-- Función logaritmo

-- La función logaritmo toma un número complejo y devuelve su logaritmo.
logaritmo :: Complejo -> Complejo
logaritmo (x, y) = (log (sqrt (x * x + y * y)), atan2 y x)

-- Función raíz cuadrada

-- La función raíz cuadrada toma un número complejo y devuelve su raíz cuadrada.
raizCuadrada :: Complejo -> Complejo
raizCuadrada (x, y) = (sqrt ((x + sqrt (x * x + y * y)) / 2), sqrt((-x + sqrt (x * x + y * y)) / 2))

-- Función conjugado

-- La función conjugado toma un número complejo y devuelve su conjugado.
conjugado :: Complejo -> Complejo
conjugado (x, y) = (x, -y)

-- Función inversa

-- La función inversa toma un número complejo y devuelve su inversa.
inversa :: Complejo -> Complejo
inversa (x, y) = (x / (x * x + y * y), -y / (x * x + y * y))

-- Función imprimir

-- La función imprimir toma un número complejo y lo imprime por pantalla.
imprimir :: Complejo -> IO ()
imprimir (x, y) = putStrLn (show x ++ " + " ++ show y ++ "i")

-- Función main

-- La función main es la función principal del programa.
main :: IO ()
main = do
  -- Creamos dos números complejos.
  c1 <- readLn
  c2 <- readLn

  -- Imprimimos los números complejos.
  imprimir c1
  imprimir c2

  -- Sumamos los números complejos.
  c3 <- sumar c1 c2

  -- Imprimimos la suma de los números complejos.
  imprimir c3

  -- Restamos los números complejos.
  c4 <- restar c1 c2

  -- Imprimimos la resta de los números complejos.
  imprimir c4

  -- Multiplicamos los números complejos.
  c5 <- multiplicar c1 c2

  -- Imprimimos el producto de los números complejos.
  imprimir c5

  -- Dividimos los números complejos.
  c6 <- dividir c1 c2

  -- Imprimimos la división de los números complejos.
  imprimir c6

  -- Calculamos el módulo de los números complejos.
  m1 <- modulo c1
  m2 <- modulo