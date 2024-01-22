```haskell
-- Definición de tipos de datos

-- Definimos un tipo de datos algebraico para representar expresiones matemáticas.
data Expr = Const Int
          | Var String
          | Neg Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr

-- Definimos un tipo de datos algebraico para representar el entorno, que es un mapa de variables a valores.
type Entorno = [(String, Int)]

-- Definición de funciones

-- Definimos una función para evaluar una expresión en un entorno.
eval :: Expr -> Entorno -> Int
eval (Const n) _ = n
eval (Var x) env = case lookup x env of
                    Just n -> n
                    Nothing -> error "Variable no definida"
eval (Neg e) env = -eval e env
eval (Add e1 e2) env = eval e1 env + eval e2 env
eval (Sub e1 e2) env = eval e1 env - eval e2 env
eval (Mul e1 e2) env = eval e1 env * eval e2 env
eval (Div e1 e2) env = eval e1 env `div` eval e2 env

-- Definimos una función para simplificar una expresión.
simplify :: Expr -> Expr
simplify (Const n) = Const n
simplify (Var x) = Var x
simplify (Neg e) = case simplify e of
                    Const n -> Const (-n)
                    Neg e' -> simplify e'
                    _ -> Neg (simplify e)
simplify (Add e1 e2) = case (simplify e1, simplify e2) of
                    (Const n1, Const n2) -> Const (n1 + n2)
                    (Const 0, e') -> simplify e'
                    (e', Const 0) -> simplify e'
                    _ -> Add (simplify e1) (simplify e2)
simplify (Sub e1 e2) = case (simplify e1, simplify e2) of
                    (Const n1, Const n2) -> Const (n1 - n2)
                    (Const 0, e') -> Neg (simplify e')
                    _ -> Sub (simplify e1) (simplify e2)
simplify (Mul e1 e2) = case (simplify e1, simplify e2) of
                    (Const n1, Const n2) -> Const (n1 * n2)
                    (Const 0, _) -> Const 0
                    (_, Const 0) -> Const 0
                    (Const 1, e') -> simplify e'
                    (e', Const 1) -> simplify e'
                    _ -> Mul (simplify e1) (simplify e2)
simplify (Div e1 e2) = case (simplify e1, simplify e2) of
                    (Const n1, Const n2) -> Const (n1 `div` n2)
                    (Const 0, _) -> Const 0
                    (_, Const 1) -> simplify e1
                    _ -> Div (simplify e1) (simplify e2)

-- Definimos una función para imprimir una expresión.
showExpr :: Expr -> String
showExpr (Const n) = show n
showExpr (Var x) = x
showExpr (Neg e) = "-" ++ showExpr e
showExpr (Add e1 e2) = "(" ++ showExpr e1 ++ " + " ++ showExpr e2 ++ ")"
showExpr (Sub e1 e2) = "(" ++ showExpr e1 ++ " - " ++ showExpr e2 ++ ")"
showExpr (Mul e1 e2) = "(" ++ showExpr e1 ++ " * " ++ showExpr e2 ++ ")"
showExpr (Div e1 e2) = "(" ++ showExpr e1 ++ " / " ++ showExpr e2 ++ ")"

-- Programa principal

-- Creamos un entorno con algunas variables definidas.
env = [("x", 3), ("y", 5)]

-- Creamos una expresión.
expr = Add (Mul (Var "x") (Const 2)) (Neg (Var "y"))

-- Evaluamos la expresión en el entorno.
result = eval expr env

-- Imprimimos el resultado.
putStrLn ("El resultado de evaluar la expresión " ++ showExpr expr ++ " en el entorno " ++ show env ++ " es " ++ show result)
```

Este código define varios tipos de datos y funciones para representar y manipular expresiones matemáticas. El tipo de datos Expr representa expresiones matemáticas, y el tipo de datos Entorno representa el entorno, que es un mapa de variables a valores.

La función eval evalúa una expresión en un entorno, y la función simplify simplifica una expresión. La función showExpr imprime una expresión.

El programa principal crea un entorno con algunas variables definidas, crea una expresión, evalúa la expresión en el entorno e imprime el resultado.