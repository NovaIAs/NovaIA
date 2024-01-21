```haskell
-- Definición de una clase Shape con un método abstracto para calcular el área.

class Shape where
  getArea :: Shape -> Double

-- Definición de una clase Circle que hereda de la clase Shape y tiene un método para obtener el radio.

class Circle a => CircleShape a where
  getRadius :: a -> Double
  instance CircleShape Double where
    getRadius = id

-- Definición de una clase Square que hereda de la clase Shape y tiene un método para obtener el lado.

class Square a => SquareShape a where
  getSide :: a -> Double
  instance SquareShape Double where
    getSide = id

-- Definición de una función para calcular el área de un círculo.

calcAreaCircle :: Num a => a -> Double
calcAreaCircle radius = pi * radius ^ 2

-- Definición de una función para calcular el área de un cuadrado.

calcAreaSquare :: Num a => a -> Double
calcAreaSquare side = side ^ 2

-- Definición de una función genérica para calcular el área de una forma.

calcArea :: Shape a => a -> Double
calcArea shape = getArea shape

-- Definición de una función para imprimir el área de una forma.

printArea :: Show a => Shape a => a -> IO ()
printArea shape = putStrLn ("El área de la forma es " ++ show (calcArea shape))

-- Uso de las funciones definidas para calcular y mostrar el área de un círculo y un cuadrado.

main :: IO ()
main = do
  let circle = 5.0
  let square = 10.0
  printArea (circle :: Double)
  printArea (square :: Double)
```

Este código define una clase `Shape` abstracta con un método abstracto para calcular el área. Luego, define dos clases concretas `CircleShape` y `SquareShape` que heredan de `Shape` y tienen métodos para obtener el radio y el lado, respectivamente. A continuación, define funciones para calcular el área de un círculo y un cuadrado, y una función genérica para calcular el área de cualquier forma que implemente la clase `Shape`. Finalmente, define una función para imprimir el área de una forma.

En la función `main`, se crean dos variables, `circle` y `square`, que representan un círculo y un cuadrado, respectivamente. Se utilizan las funciones definidas para calcular y mostrar el área de estas formas.