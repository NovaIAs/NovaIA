```haskell
-- Definición de un tipo de datos para representar expresiones matemáticas

data Expresion = Numero Int
                | Suma Expresion Expresion
                | Resta Expresion Expresion
                | Producto Expresion Expresion
                | Division Expresion Expresion

-- Función para evaluar una expresión matemática

evaluar :: Expresion -> Int
evaluar (Numero n) = n
evaluar (Suma e1 e2) = evaluar e1 + evaluar e2
evaluar (Resta e1 e2) = evaluar e1 - evaluar e2
evaluar (Producto e1 e2) = evaluar e1 * evaluar e2
evaluar (Division e1 e2) = evaluar e1 `div` evaluar e2

-- Función para mostrar una expresión matemática

mostrar :: Expresion -> String
mostrar (Numero n) = show n
mostrar (Suma e1 e2) = "(" ++ mostrar e1 ++ " + " ++ mostrar e2 ++ ")"
mostrar (Resta e1 e2) = "(" ++ mostrar e1 ++ " - " ++ mostrar e2 ++ ")"
mostrar (Producto e1 e2) = "(" ++ mostrar e1 ++ " * " ++ mostrar e2 ++ ")"
mostrar (Division e1 e2) = "(" ++ mostrar e1 ++ " / " ++ mostrar e2 ++ ")"

-- Función para simplificar una expresión matemática

simplificar :: Expresion -> Expresion
simplificar (Suma (Numero 0) e) = simplificar e
simplificar (Suma e (Numero 0)) = simplificar e
simplificar (Resta (Numero 0) e) = simplificar (Negacion e)
simplificar (Resta e (Numero 0)) = simplificar e
simplificar (Producto (Numero 0) _) = Numero 0
simplificar (Producto _ (Numero 0)) = Numero 0
simplificar (Producto (Numero 1) e) = simplificar e
simplificar (Producto e (Numero 1)) = simplificar e
simplificar (Division (Numero 0) _) = Numero 0
simplificar (Division _ (Numero 0)) = error "División por cero"
simplificar (Suma e1 e2) = Suma (simplificar e1) (simplificar e2)
simplificar (Resta e1 e2) = Resta (simplificar e1) (simplificar e2)
simplificar (Producto e1 e2) = Producto (simplificar e1) (simplificar e2)
simplificar (Division e1 e2) = Division (simplificar e1) (simplificar e2)

-- Función para factorizar una expresión matemática

factorizar :: Expresion -> Expresion
factorizar (Suma (Producto (Numero a) e1) (Producto (Numero a) e2)) = Producto (Numero a) (Suma e1 e2)
factorizar (Suma (Producto e1 (Numero a)) (Producto e2 (Numero a))) = Producto (Numero a) (Suma e1 e2)
factorizar (Resta (Producto (Numero a) e1) (Producto (Numero a) e2)) = Producto (Numero a) (Resta e1 e2)
factorizar (Resta (Producto e1 (Numero a)) (Producto e2 (Numero a))) = Producto (Numero a) (Resta e1 e2)
factorizar e = e

-- Función para expandir una expresión matemática

expandir :: Expresion -> Expresion
expandir (Producto (Suma e1 e2) e3) = Suma (Producto e1 e3) (Producto e2 e3)
expandir (Producto e1 (Suma e2 e3)) = Suma (Producto e1 e2) (Producto e1 e3)
expandir e = e

-- Función principal

main :: IO ()
main = do
  let e = Suma (Producto (Numero 2) (Suma (Numero 3) (Numero 4))) (Producto (Numero 5) (Resta (Numero 6) (Numero 7)))
  print (mostrar e)
  print (evaluar e)
  print (mostrar (simplificar e))
  print (mostrar (factorizar e))
  print (mostrar (expandir e))
```

Este código es un programa completo de Haskell que puede ser ejecutado directamente. Define una serie de funciones para trabajar con expresiones matemáticas, incluyendo funciones para evaluar, mostrar, simplificar, factorizar y expandir expresiones. El programa principal crea una expresión matemática simple, la imprime, la evalúa, la simplifica, la factoriza y la expande, y luego imprime los resultados.

Aquí hay una explicación más detallada del código:

* La primera línea define un tipo de datos llamado `Expresion` que se utiliza para representar expresiones matemáticas. Una expresión puede ser un número, una suma, una resta, un producto o una división.
* La segunda línea define una función llamada `evaluar` que toma una expresión y devuelve su valor.
* La tercera línea define una función llamada `mostrar` que toma una expresión y devuelve una cadena de caracteres que la representa.
* La cuarta línea define una función llamada `simplificar` que toma una expresión y devuelve una expresión simplificada.
* La quinta línea define una función llamada `factorizar` que toma una expresión y devuelve una expresión factorizada.
* La sexta línea define una función llamada `expandir` que toma una expresión y devuelve una expresión expandida.
* La séptima línea define una función principal que crea una expresión matemática simple, la imprime, la evalúa, la simplifica, la factoriza y la expande, y luego imprime los resultados.