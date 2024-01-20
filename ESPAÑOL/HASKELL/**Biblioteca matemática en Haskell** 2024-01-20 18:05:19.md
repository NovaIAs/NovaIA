```haskell
import Data.List (group, sortBy)
import Data.Char (toUpper)

-- Un constructor de datos para representar una expresión matemática.
data Expresion = Numero Int
              | Variable String
              | Suma Expresion Expresion
              | Resta Expresion Expresion
              | Multiplicacion Expresion Expresion
              | Division Expresion Expresion

-- Una función para evaluar una expresión matemática.
evaluar :: Expresion -> Int
evaluar (Numero n) = n
evaluar (Variable x) = error "Variable no definida"
evaluar (Suma e1 e2) = evaluar e1 + evaluar e2
evaluar (Resta e1 e2) = evaluar e1 - evaluar e2
evaluar (Multiplicacion e1 e2) = evaluar e1 * evaluar e2
evaluar (Division e1 e2) = evaluar e1 `div` evaluar e2

-- Una función para imprimir una expresión matemática.
imprimir :: Expresion -> String
imprimir (Numero n) = show n
imprimir (Variable x) = x
imprimir (Suma e1 e2) = "(" ++ imprimir e1 ++ " + " ++ imprimir e2 ++ ")"
imprimir (Resta e1 e2) = "(" ++ imprimir e1 ++ " - " ++ imprimir e2 ++ ")"
imprimir (Multiplicacion e1 e2) = "(" ++ imprimir e1 ++ " * " ++ imprimir e2 ++ ")"
imprimir (Division e1 e2) = "(" ++ imprimir e1 ++ " / " ++ imprimir e2 ++ ")"

-- Una función para simplificar una expresión matemática.
simplificar :: Expresion -> Expresion
simplificar (Suma (Numero n1) (Numero n2)) = Numero (n1 + n2)
simplificar (Resta (Numero n1) (Numero n2)) = Numero (n1 - n2)
simplificar (Multiplicacion (Numero n1) (Numero n2)) = Numero (n1 * n2)
simplificar (Division (Numero n1) (Numero n2)) = Numero (n1 `div` n2)
simplificar (Suma e1 e2) = Suma (simplificar e1) (simplificar e2)
simplificar (Resta e1 e2) = Resta (simplificar e1) (simplificar e2)
simplificar (Multiplicacion e1 e2) = Multiplicacion (simplificar e1) (simplificar e2)
simplificar (Division e1 e2) = Division (simplificar e1) (simplificar e2)

-- Una función para factorizar una expresión matemática.
factorizar :: Expresion -> Expresion
factorizar (Suma e1 e2) = Suma (factorizar e1) (factorizar e2)
factorizar (Resta e1 e2) = Resta (factorizar e1) (factorizar e2)
factorizar (Multiplicacion e1 e2) =
  case (factorizar e1, factorizar e2) of
    (Numero n1, Numero n2) -> Numero (n1 * n2)
    (Numero n1, e2') -> Multiplicacion (Numero n1) e2'
    (e1', Numero n2) -> Multiplicacion e1' (Numero n2)
    (e1', e2') -> Multiplicacion e1' e2'
factorizar (Division e1 e2) = Division (factorizar e1) (factorizar e2)

-- Una función para resolver una ecuación matemática.
resolver :: Expresion -> Expresion -> Expresion
resolver (Variable x) e2 = e2
resolver e1 (Variable x) = e1
resolver (Suma e1 e2) (Suma e3 e4) = resolver e1 e3 `Suma` resolver e2 e4
resolver (Resta e1 e2) (Resta e3 e4) = resolver e1 e3 `Resta` resolver e2 e4
resolver (Multiplicacion e1 e2) (Multiplicacion e3 e4) = resolver e1 e3 `Multiplicacion` resolver e2 e4
resolver (Division e1 e2) (Division e3 e4) = resolver e1 e3 `Division` resolver e2 e4
resolver e1 e2 = error "Ecuación no soluble"

-- Una función para encontrar las raíces de una ecuación cuadrática.
raices :: Expresion -> [Expresion]
raices (Suma (Multiplicacion (Numero a) (Variable x)) (Multiplicacion (Numero b) (Variable x))) (Suma (Numero c) (Multiplicacion (Numero d) (Variable x)))) =
  let discriminante = b^2 - 4 * a * c
  in  if discriminante < 0 then []
      else [(-b + sqrt discriminante) `Division` (2 * a), (-b - sqrt discriminante) `Division` (2 * a)]
raices _ _ = []

-- Una función para encontrar los máximos y mínimos de una función matemática.
maximosMinimos :: Expresion -> [Expresion]
maximosMinimos (Suma e1 e2) = maximosMinimos e1 ++ maximosMinimos e2
maximosMinimos (Resta e1 e2) = maximosMinimos e1 ++ maximosMinimos e2
maximosMinimos (Multiplicacion e1 e2) = maximosMinimos e1 ++ maximosMinimos e2
maximosMinimos (Division e1 e2) = maximosMinimos e1 ++ maximosMinimos e2
maximosMinimos (Numero n) = []
maximosMinimos (Variable x) = []

-- Una función para encontrar el área de una figura geométrica.
area :: Expresion -> Int
area (Suma e1 e2) = area e1 + area e2
area (Resta e1 e2) = area e1 - area e2
area (Multiplicacion e1 e2) = area e1 * area e2
area (Division e