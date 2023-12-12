Claro! Aqui está um código complexo em Lua que realiza uma simulação de um sistema de gerenciamento de estoque de uma loja fictícia. Ele inclui funcionalidades como adicionar produtos, atualizar estoque, visualizar produtos em baixa quantidade, realizar vendas e gerar relatórios. 

```lua
-- Definição da tabela que irá armazenar os produtos
local estoque = {}

-- Função para adicionar um produto ao estoque
local function adicionarProduto(nome, quantidade)
  estoque[nome] = quantidade
end

-- Função para atualizar a quantidade de um produto no estoque
local function atualizarEstoque(nome, quantidade)
  if estoque[nome] then
    estoque[nome] = estoque[nome] + quantidade
  else
    print("Produto não encontrado.")
  end
end

-- Função para verificar quais produtos estão em baixa quantidade no estoque
local function verificarBaixaQuantidade()
  print("Produtos em baixa quantidade:")
  for produto, quantidade in pairs(estoque) do
    if quantidade < 10 then
      print(produto .. ": " .. quantidade)
    end
  end
end

-- Função para realizar uma venda de um produto
local function realizarVenda(nome, quantidade)
  if estoque[nome] and estoque[nome] >= quantidade then
    estoque[nome] = estoque[nome] - quantidade
    print("Venda realizada com sucesso!")
  else
    print("Produto indisponível no estoque.")
  end
end

-- Função para gerar um relatório com o estoque atual
local function gerarRelatorio()
  print("Relatório de estoque:")
  for produto, quantidade in pairs(estoque) do
    print(produto .. ": " .. quantidade)
  end
end

-- Exemplo de utilização das funções do sistema
adicionarProduto("Camiseta", 20)
adicionarProduto("Calça", 15)
adicionarProduto("Tênis", 10)

atualizarEstoque("Camiseta", 5)
atualizarEstoque("Calça", -3)

verificarBaixaQuantidade()

realizarVenda("Camiseta", 3)
realizarVenda("Calça", 10)

gerarRelatorio()
```

Explicação do código:
- Primeiro, definimos uma tabela chamada "estoque" para armazenar os produtos e suas quantidades.
- Em seguida, temos a função "adicionarProduto" que recebe o nome e a quantidade de um produto e o adiciona ao estoque.
- A função "atualizarEstoque" permite alterar a quantidade de um produto no estoque, adicionando ou subtraindo uma determinada quantidade.
- A função "verificarBaixaQuantidade" percorre o estoque e exibe os produtos que possuem uma quantidade inferior a 10 unidades.
- A função "realizarVenda" permite realizar a venda de um produto, subtraindo a quantidade vendida do estoque, desde que o produto esteja disponível em quantidade suficiente.
- Por fim, a função "gerarRelatorio" exibe um relatório com o estoque atual, mostrando todos os produtos e suas respectivas quantidades.

No exemplo de utilização, adicionamos alguns produtos ao estoque, atualizamos a quantidade de alguns deles, verificamos quais estão em baixa quantidade, realizamos vendas e, por fim, geramos o relatório com o estoque atualizado.