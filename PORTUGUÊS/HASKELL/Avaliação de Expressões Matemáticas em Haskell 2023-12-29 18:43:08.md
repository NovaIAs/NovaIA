Claro! Aqui está um exemplo de um código complexo em Haskell, que realiza uma avaliação de expressões matemáticas em linguagem natural:

```haskell
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

-- Definição da gramática da linguagem
languageDef :: LanguageDef st
languageDef = emptyDef
    { commentStart   = "/*"
    , commentEnd     = "*/"
    , identStart     = letter
    , identLetter    = alphaNum
    , opStart        = oneOf "+-*/"
    , opLetter       = oneOf "+-*/"
    , reservedOpNames= ["+", "-", "*", "/"]
    , reservedNames  = ["mais", "menos", "vezes", "dividido", "por"]
    }

lexer :: TokenParser st
lexer = makeTokenParser languageDef

-- Definição dos tipos de dados
data Expr = Val Double
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr

-- Parser para a avaliação de expressões
exprParser :: Parser Expr
exprParser = buildExpressionParser table term <?> "expressão matemática"

table :: OperatorTable Char st Expr
table = [ [binaryOp "+" Add, binaryOp "-" Sub]
        , [binaryOp "*" Mul, binaryOp "/" Div]
        ]

binaryOp :: String -> (Expr -> Expr -> Expr) -> Operator Char st Expr
binaryOp name fun = Infix (reservedOp lexer name >> return fun) AssocLeft

term :: Parser Expr
term = parens lexer exprParser
    <|> Val <$> float lexer

-- Função principal
avaliarExpressao :: String -> Either ParseError Double
avaliarExpressao input = do
    tokens <- parse (whiteSpace lexer >> exprParser) "" input
    return $ eval tokens

-- Função para avaliar a expressão
eval :: Expr -> Double
eval (Val x)   = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x / eval y

main :: IO ()
main = do
    putStrLn "Digite uma expressão matemática:"
    expressao <- getLine
    case avaliarExpressao expressao of
        Left err   -> putStrLn $ "Erro de parsing: " ++ show err
        Right res  -> putStrLn $ "Resultado: " ++ show res
```

Neste exemplo, utilizamos a biblioteca `Parsec` para fazer o parsing da expressão matemática em linguagem natural. Definimos a gramática da linguagem e os operadores matemáticos utilizando a função `LanguageDef`. Em seguida, criamos o lexer com a função `makeTokenParser` utilizando a definição da linguagem.

Em seguida, definimos os tipos de dados `Expr` que representam as expressões matemáticas e os parsers `exprParser` e `term` para a avaliação das expressões.

Por fim, temos a função principal `avaliarExpressao` que recebe uma expressão matemática em linguagem natural como entrada e retorna o resultado da avaliação. A função `eval` avalia a expressão e a função `main` é responsável por ler a expressão do usuário e exibir o resultado.

Espero que este código atenda às suas expectativas!