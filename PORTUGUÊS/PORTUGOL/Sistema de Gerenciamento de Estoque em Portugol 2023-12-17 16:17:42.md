Claro! Vou gerar um código complexo em Portugol para você. O código a seguir é um exemplo de um sistema de gerenciamento de estoque de uma loja.

```
algoritmo GerenciamentoEstoque

// Definição das constantes
const MAX_PRODUTOS <- 100
const MAX_VENDAS <- 1000

// Definição do registro de produtos
registro Produto
    codigo: inteiro
    nome: cadeia
    quantidade: inteiro
    preco: real
fim_registro

// Definição do registro de vendas
registro Venda
    codigoProduto: inteiro
    quantidadeVendida: inteiro
    valorTotal: real
fim_registro

// Definição do vetor de produtos
produtos: vetor [1..MAX_PRODUTOS] de Produto

// Definição do vetor de vendas
vendas: vetor [1..MAX_VENDAS] de Venda

// Definição das variáveis
totalProdutos: inteiro
totalVendas: inteiro
opcao: inteiro

// Inicialização das variáveis
totalProdutos <- 0
totalVendas <- 0

// Função para exibir o menu de opções
funcao exibirMenu() : inteiro
    escreva("---------- MENU ----------")
    escreva("1. Cadastrar produto")
    escreva("2. Registrar venda")
    escreva("3. Consultar estoque")
    escreva("4. Consultar vendas")
    escreva("5. Sair")
    escreva("---------------------------")
    escreva("Escolha uma opcao:")
    leia(opcao)
    retorne opcao
fim_funcao

// Função para cadastrar um produto
funcao cadastrarProduto()
    se totalProdutos < MAX_PRODUTOS entao
        totalProdutos <- totalProdutos + 1
        escreva("----- CADASTRO DE PRODUTO -----")
        escreva("Codigo do produto:")
        leia(produtos[totalProdutos].codigo)
        escreva("Nome do produto:")
        leia(produtos[totalProdutos].nome)
        escreva("Quantidade do produto:")
        leia(produtos[totalProdutos].quantidade)
        escreva("Preco do produto:")
        leia(produtos[totalProdutos].preco)
        escreva("Produto cadastrado com sucesso!")
        escreva("--------------------------------")
    senao
        escreva("Nao foi possivel cadastrar o produto. Limite maximo de produtos atingido.")
    fim_se
fim_funcao

// Função para registrar uma venda
funcao registrarVenda()
    se totalProdutos > 0 entao
        totalVendas <- totalVendas + 1
        escreva("----- REGISTRO DE VENDA -----")
        escreva("Codigo do produto:")
        leia(vendas[totalVendas].codigoProduto)
        escreva("Quantidade vendida:")
        leia(vendas[totalVendas].quantidadeVendida)
        vendas[totalVendas].valorTotal <- vendas[totalVendas].quantidadeVendida * produtos[vendas[totalVendas].codigoProduto].preco
        produtos[vendas[totalVendas].codigoProduto].quantidade <- produtos[vendas[totalVendas].codigoProduto].quantidade - vendas[totalVendas].quantidadeVendida
        escreva("Venda registrada com sucesso!")
        escreva("----------------------------")
    senao
        escreva("Nao foi possivel registrar a venda. Nenhum produto cadastrado.")
    fim_se
fim_funcao

// Função para consultar o estoque
funcao consultarEstoque()
    escreva("----- CONSULTA DE ESTOQUE -----")
    se totalProdutos > 0 entao
        para i de 1 ate totalProdutos faca
            escreva("Codigo:", produtos[i].codigo)
            escreva("Nome:", produtos[i].nome)
            escreva("Quantidade:", produtos[i].quantidade)
            escreva("Preco:", produtos[i].preco)
            escreva("-----------------------------")
        fim_para
    senao
        escreva("Nenhum produto cadastrado.")
    fim_se
fim_funcao

// Função para consultar as vendas
funcao consultarVendas()
    escreva("----- CONSULTA DE VENDAS -----")
    se totalVendas > 0 entao
        para i de 1 ate totalVendas faca
            escreva("Codigo do produto:", vendas[i].codigoProduto)
            escreva("Quantidade vendida:", vendas[i].quantidadeVendida)
            escreva("Valor total:", vendas[i].valorTotal)
            escreva("-----------------------------")
        fim_para
    senao
        escreva("Nenhuma venda registrada.")
    fim_se
fim_funcao

// Início do programa principal
inicio
    repita
        opcao <- exibirMenu()
        escolha opcao
            caso 1
                cadastrarProduto()
            caso 2
                registrarVenda()
            caso 3
                consultarEstoque()
            caso 4
                consultarVendas()
            caso 5
                escreva("Saindo...")
            outro_caso
                escreva("Opcao invalida.")
        fim_escolha
    ate opcao = 5
fim
```

Este código apresenta um menu com as opções de cadastrar produto, registrar venda, consultar estoque, consultar vendas e sair. Cada opção tem sua respectiva função para executar as operações desejadas.

A função `cadastrarProduto` permite cadastrar um novo produto, solicitando o código, nome, quantidade e preço do produto. O produto é armazenado no vetor `produtos`.

A função `registrarVenda` permite registrar uma venda, solicitando o código do produto e a quantidade vendida. A função calcula o valor total da venda e atualiza a quantidade em estoque do produto correspondente.

A função `consultarEstoque` mostra todos os produtos cadastrados, exibindo o código, nome, quantidade e preço de cada um.

A função `consultarVendas` mostra todas as vendas registradas, exibindo o código do produto, a quantidade vendida e o valor total de cada uma.

O programa principal utiliza um loop para exibir o menu de opções até que o usuário escolha a opção de sair.

Espero que este código atenda às suas expectativas!