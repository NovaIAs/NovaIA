**Programme à facteur**

```haskell
-- Type des expressions
data Expr = Val Int
          | Plus Expr Expr
          | Mult Expr Expr
          | Neg Expr
          | Div Expr Expr

-- Évaluation d'une expression
eval :: Expr -> Int
eval (Val v) = v
eval (Plus e1 e2) = eval e1 + eval e2
eval (Mult e1 e2) = eval e1 * eval e2
eval (Neg e) = - eval e
eval (Div e1 e2) = eval e1 `div` eval e2

-- Conversion d'une chaîne en expression
parse :: String -> Maybe Expr
parse s = case reads s :: [(Int, String)] of
  [(v, "")] -> Just $ Val v
  [(v, s')] -> do e <- parse s'; return $ Plus (Val v) e
  _ -> Nothing

-- Affichage d'une expression
showExpr :: Expr -> String
showExpr (Val v) = show v
showExpr (Plus e1 e2) = "(" ++ showExpr e1 ++ " + " ++ showExpr e2 ++ ")"
showExpr (Mult e1 e2) = "(" ++ showExpr e1 ++ " * " ++ showExpr e2 ++ ")"
showExpr (Neg e) = "(-" ++ showExpr e ++ ")"
showExpr (Div e1 e2) = "(" ++ showExpr e1 ++ " / " ++ showExpr e2 ++ ")"
```

**Fonction principale**

```haskell
main :: IO ()
main = do
  putStrLn "Calculatrice à facteurs simple"
  putStr "Entrez une expression : "
  line <- getLine
  case parse line of
    Just e -> putStrLn $ "Résultat : " ++ show (eval e)
    Nothing -> putStrLn "Expression invalide"
```

**Explication**

Ce programme implémente une calculatrice de base qui peut évaluer des expressions à facteurs.

**Type d'expression**

Le type `Expr` représente les expressions qui peuvent être évaluées. Il comprend des constructeurs pour les valeurs, les additions, les multiplications, les négations et les divisions.

**Évaluation d'une expression**

La fonction `eval` évalue une expression en appliquant récursivement les opérations correspondantes aux constructeurs de l'expression.

**Conversion d'une chaîne en expression**

La fonction `parse` convertit une chaîne en une expression valide. Elle utilise la fonction `reads` pour extraire une valeur entière de la chaîne et construire une expression `Val` correspondante. Si la chaîne contient un caractère `+`, elle construit une expression `Plus` en appelant récursivement `parse` sur la partie restante de la chaîne.

**Affichage d'une expression**

La fonction `showExpr` affiche une expression sous forme de chaîne. Elle utilise des parenthèses pour l'ambiguïté, en particulier pour la division.

**Fonction principale**

La fonction `main` est le point d'entrée du programme. Elle invite l'utilisateur à entrer une expression, la parse, l'évalue et affiche le résultat.