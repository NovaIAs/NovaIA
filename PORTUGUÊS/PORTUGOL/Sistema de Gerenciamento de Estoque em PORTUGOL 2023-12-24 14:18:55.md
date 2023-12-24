Claro! Vou gerar um código em PORTUGOL complexo e único para você. Este código é um exemplo de um sistema de gerenciamento de estoque de uma loja fictícia. Ele permite adicionar produtos, realizar vendas, gerar relatórios de vendas e estoque, e também oferece a opção de exclusão de produtos. Vamos lá:

```
algoritmo SistemaGerenciamentoEstoque

var
   produtos: vetor [1..100] de RegistroProduto
   totalProdutos: inteiro
   vendas: vetor [1..1000] de RegistroVenda
   totalVendas: inteiro

registro RegistroProduto
   codigo: inteiro
   nome: caractere[50]
   quantidade: inteiro
   preco: real

registro RegistroVenda
   codigoProduto: inteiro
   quantidadeVendida: inteiro
   dataVenda: caractere[10]

procedimento AdicionarProduto()
   var
      novoProduto: RegistroProduto

   escreva("Digite o código do produto: ")
   leia(novoProduto.codigo)
   escreva("Digite o nome do produto: ")
   leia(novoProduto.nome)
   escreva("Digite a quantidade do produto: ")
   leia(novoProduto.quantidade)
   escreva("Digite o preço do produto: ")
   leia(novoProduto.preco)

   totalProdutos <- totalProdutos + 1
   produtos[totalProdutos] <- novoProduto

procedimento RealizarVenda()
   var
      novoVenda: RegistroVenda

   escreva("Digite o código do produto vendido: ")
   leia(novoVenda.codigoProduto)
   escreva("Digite a quantidade vendida: ")
   leia(novoVenda.quantidadeVendida)
   escreva("Digite a data da venda (dd/mm/aaaa): ")
   leia(novoVenda.dataVenda)

   totalVendas <- totalVendas + 1
   vendas[totalVendas] <- novoVenda

procedimento ExcluirProduto(codigoProduto: inteiro)
   var
      encontrado: booleano
      i: inteiro

   encontrado <- falso
   i <- 1

   enquanto i <= totalProdutos E NÃO encontrado faça
      se produtos[i].codigo = codigoProduto então
         encontrado <- verdadeiro
         produtos[i] <- produtos[totalProdutos]
         totalProdutos <- totalProdutos - 1
      fimse
      i <- i + 1
   fimenquanto

   se encontrado então
      escreva("Produto excluído com sucesso!")
   senão
      escreva("Produto não encontrado.")
   fimse

procedimento GerarRelatorioVendas()
   var
      totalVendido: real
      i: inteiro

   totalVendido <- 0

   para i de 1 até totalVendas faça
      para j de 1 até totalProdutos faça
         se vendas[i].codigoProduto = produtos[j].codigo então
            totalVendido <- totalVendido + (vendas[i].quantidadeVendida * produtos[j].preco)
         fimse
      fimpara
   fimpara

   escreva("Total vendido: R$", totalVendido)

procedimento GerarRelatorioEstoque()
   var
      i: inteiro

   para i de 1 até totalProdutos faça
      escreva("Código: ", produtos[i].codigo)
      escreva("Nome: ", produtos[i].nome)
      escreva("Quantidade: ", produtos[i].quantidade)
      escreva("Preço: R$", produtos[i].preco)
   fimpara

inicio
   totalProdutos <- 0
   totalVendas <- 0

   repita
      escreva("===== Menu Principal =====")
      escreva("1 - Adicionar Produto")
      escreva("2 - Realizar Venda")
      escreva("3 - Excluir Produto")
      escreva("4 - Gerar Relatório de Vendas")
      escreva("5 - Gerar Relatório de Estoque")
      escreva("0 - Sair")
      escreva("==========================")
      escreva("Digite a opção desejada: ")
      leia(opcao)

      escolha opcao
         caso 1
            AdicionarProduto()
         caso 2
            RealizarVenda()
         caso 3
            escreva("Digite o código do produto a ser excluído: ")
            leia(codigo)
            ExcluirProduto(codigo)
         caso 4
            GerarRelatorioVendas()
         caso 5
            GerarRelatorioEstoque()
         caso 0
            escreva("Saindo...")
         senão
            escreva("Opção inválida.")
      fimescolha

   até opcao = 0

fimalgoritmo
```

Este código em PORTUGOL implementa um sistema de gerenciamento de estoque completo, com funcionalidades para adicionar produtos, realizar vendas, excluir produtos, gerar relatórios de vendas e estoque. Espero que seja útil para o seu aprendizado e desenvolvimento!