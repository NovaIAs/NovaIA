Claro! Aqui está um código complexo em Haskell que envolve várias funcionalidades e explicações detalhadas:

```haskell
import Data.List

-- Definindo o tipo de dado para representar um carro
data Carro = Carro {
    marca :: String,
    modelo :: String,
    ano :: Int,
    preco :: Float
} deriving (Show)

-- Função para calcular o preço médio dos carros de uma determinada marca
precoMedioPorMarca :: String -> [Carro] -> Float
precoMedioPorMarca marca carros =
    let
        carrosFiltrados = filter (\c -> (marca c) == marca) carros
        precos = map preco carrosFiltrados
        soma = foldl (+) 0 precos
        quantidade = length carrosFiltrados
    in
        if quantidade == 0
            then 0
            else soma / (fromIntegral quantidade)

-- Função para encontrar o carro mais caro
carroMaisCaro :: [Carro] -> Carro
carroMaisCaro carros =
    let
        carrosOrdenados = sortBy (\c1 c2 -> compare (preco c1) (preco c2)) carros
    in
        last carrosOrdenados

-- Função para encontrar o modelo mais recente
modeloMaisRecente :: [Carro] -> String
modeloMaisRecente carros =
    let
        carrosOrdenados = sortBy (\c1 c2 -> compare (ano c2) (ano c1)) carros
    in
        modelo (head carrosOrdenados)

-- Função para calcular o desconto em porcentagem para um determinado valor
calcularDesconto :: Float -> Float -> Float
calcularDesconto valor desconto =
    valor * (1 - (desconto / 100))

-- Função para calcular o valor final de um carro com desconto
calcularValorFinal :: Carro -> Float -> Float
calcularValorFinal carro desconto =
    let
        valorDescontado = calcularDesconto (preco carro) desconto
    in
        if valorDescontado < 0
            then 0
            else valorDescontado

-- Exemplo de utilização dos códigos acima
main :: IO ()
main = do
    let carros = [
            Carro "Fiat" "Palio" 2010 25000.0,
            Carro "Chevrolet" "Onix" 2015 35000.0,
            Carro "Volkswagen" "Gol" 2005 20000.0,
            Carro "Fiat" "Uno" 2012 28000.0,
            Carro "Chevrolet" "Cruze" 2020 50000.0
        ]
    putStrLn "Preço médio dos carros Fiat:"
    putStrLn $ show (precoMedioPorMarca "Fiat" carros)
    putStrLn "Carro mais caro:"
    putStrLn $ show (carroMaisCaro carros)
    putStrLn "Modelo mais recente:"
    putStrLn $ modeloMaisRecente carros
    putStrLn "Valor final do Fiat Uno com 10% de desconto:"
    putStrLn $ show (calcularValorFinal (head carros) 10)
```

Neste código em Haskell, criamos um tipo de dado `Carro` que representa as informações de um carro, como marca, modelo, ano e preço. Em seguida, definimos algumas funções:

- `precoMedioPorMarca`: recebe uma marca e uma lista de carros, filtra os carros da marca especificada e calcula o preço médio desses carros.
- `carroMaisCaro`: recebe uma lista de carros e retorna o carro com o preço mais alto.
- `modeloMaisRecente`: recebe uma lista de carros e retorna o modelo do carro mais recente.
- `calcularDesconto`: recebe um valor e um desconto, e retorna o valor com o desconto aplicado.
- `calcularValorFinal`: recebe um carro e um desconto, e retorna o valor final do carro com o desconto aplicado.

Por fim, no exemplo de utilização, criamos uma lista de carros e chamamos as funções para obter diferentes informações, como o preço médio dos carros da marca Fiat, o carro mais caro da lista, o modelo mais recente e o valor final de um carro com desconto.

Espero que este código complexo em Haskell atenda às suas expectativas!