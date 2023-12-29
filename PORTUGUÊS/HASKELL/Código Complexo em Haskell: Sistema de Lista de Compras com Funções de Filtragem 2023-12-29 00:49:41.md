Claro! Aqui está um código complexo em Haskell que utiliza várias funcionalidades da linguagem:

```haskell
import qualified Data.Map as Map

-- Definindo um tipo de dado para representar um usuário
data Usuario = Usuario { nome :: String, idade :: Int, email :: String } deriving (Show)

-- Definindo um tipo de dado para representar um item de uma lista de compras
data Item = Item { nomeItem :: String, preco :: Float } deriving (Show)

-- Definindo um tipo de dado para representar a lista de compras de um usuário
type ListaCompras = Map.Map Usuario [Item]

-- Função para adicionar um item na lista de compras de um usuário
adicionarItem :: Usuario -> Item -> ListaCompras -> ListaCompras
adicionarItem usuario item lista = Map.insertWith (++) usuario [item] lista

-- Função para calcular o valor total de uma lista de compras de um usuário
calcularValorTotal :: Usuario -> ListaCompras -> Float
calcularValorTotal usuario lista = case Map.lookup usuario lista of
                                     Just itens -> sum [preco item | item <- itens]
                                     Nothing    -> 0

-- Função para verificar se um usuário é maior de idade
ehMaiorDeIdade :: Usuario -> Bool
ehMaiorDeIdade usuario = idade usuario >= 18

-- Função para filtrar os usuários maiores de idade e calcular o valor total de suas compras
filtrarUsuariosMaioresDeIdadeECalcularValorTotal :: ListaCompras -> [(Usuario, Float)]
filtrarUsuariosMaioresDeIdadeECalcularValorTotal lista = [(usuario, calcularValorTotal usuario lista) | (usuario, _) <- Map.toList lista, ehMaiorDeIdade usuario]

-- Exemplo de uso:
main :: IO ()
main = do
  let listaCompras = adicionarItem (Usuario "Joao" 25 "joao@email.com") (Item "Camiseta" 50.0) Map.empty
  let novaListaCompras = adicionarItem (Usuario "Maria" 17 "maria@email.com") (Item "Calca" 80.0) listaCompras
  print $ filtrarUsuariosMaioresDeIdadeECalcularValorTotal novaListaCompras
```

Neste código, definimos dois tipos de dados (`Usuario` e `Item`) e um tipo sinônimo (`ListaCompras`). Em seguida, temos a função `adicionarItem`, que permite adicionar um item na lista de compras de um usuário. A função `calcularValorTotal` recebe um usuário e uma lista de compras e retorna o valor total dos itens dessa lista. A função `ehMaiorDeIdade` verifica se um usuário é maior de idade. 

Por fim, temos a função `filtrarUsuariosMaioresDeIdadeECalcularValorTotal`, que filtra os usuários maiores de idade da lista de compras e calcula o valor total de suas compras. No exemplo de uso, criamos uma lista de compras, adicionamos itens para dois usuários diferentes e imprimimos o resultado da função `filtrarUsuariosMaioresDeIdadeECalcularValorTotal`.