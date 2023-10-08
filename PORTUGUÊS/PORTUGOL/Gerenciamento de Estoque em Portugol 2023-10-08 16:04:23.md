Claro! Vou criar um código complexo em Portugol para você. Este código irá implementar um sistema de gerenciamento de estoque de uma loja, com funcionalidades de cadastro de produtos, controle de estoque, vendas e relatórios. Vou dividi-lo em partes para facilitar a explicação.

1. Declaração de variáveis e constantes:
```
algoritmo "Gerenciamento de Estoque"
var
   estoque: vetor[1..100] de registro
      codigo: inteiro
      nome: string
      quantidade: inteiro
      preco: real
      vendas: inteiro
   opcao: inteiro
   totalVendas: real
   totalProdutos: inteiro
   mediaPreco: real
   i: inteiro
const
   MAX_PRODUTOS = 100
```
Nesta primeira parte, criamos as variáveis e constantes necessárias. A variável "estoque" é um vetor que armazena registros com informações sobre os produtos, como código, nome, quantidade, preço e vendas. A variável "opcao" será utilizada para armazenar a opção escolhida pelo usuário no menu. As variáveis "totalVendas", "totalProdutos" e "mediaPreco" serão utilizadas para calcular estatísticas sobre o estoque. A variável "i" será utilizada como contador em loops. A constante "MAX_PRODUTOS" define o tamanho máximo do vetor "estoque".

2. Menu de opções:
```
procedimento exibirMenu()
inicio
   escreva("=== Menu ===")
   escreva("1. Cadastrar produto")
   escreva("2. Vender produto")
   escreva("3. Relatórios")
   escreva("4. Sair")
   escreva("Escolha uma opção: ")
   leia(opcao)
fim
```
Nesta parte, criamos um procedimento chamado "exibirMenu" que irá mostrar as opções disponíveis para o usuário. Ele irá escrever o menu na tela e ler a opção escolhida pelo usuário.

3. Cadastro de produtos:
```
procedimento cadastrarProduto()
inicio
   se totalProdutos < MAX_PRODUTOS então
      escreva("Digite o código do produto: ")
      leia(estoque[totalProdutos+1].codigo)
      escreva("Digite o nome do produto: ")
      leia(estoque[totalProdutos+1].nome)
      escreva("Digite a quantidade em estoque: ")
      leia(estoque[totalProdutos+1].quantidade)
      escreva("Digite o preço: ")
      leia(estoque[totalProdutos+1].preco)
      estoque[totalProdutos+1].vendas <- 0
      totalProdutos <- totalProdutos + 1
   senao
      escreva("Limite de produtos atingido!")
   fimse
fim
```
Neste procedimento, o usuário poderá cadastrar novos produtos. Se o número total de produtos ainda não atingiu o limite máximo, o usuário será solicitado a digitar o código, nome, quantidade em estoque e preço do produto. Será criado um novo registro na posição "totalProdutos+1" do vetor "estoque" e o total de produtos será incrementado. Caso contrário, será exibida uma mensagem informando que o limite foi atingido.

4. Venda de produtos:
```
procedimento venderProduto()
inicio
   escreva("Digite o código do produto a ser vendido: ")
   leia(codigo)
   i <- 1
   enquanto i <= totalProdutos faça
      se estoque[i].codigo = codigo então
         se estoque[i].quantidade > 0 então
            escreva("Digite a quantidade a ser vendida: ")
            leia(quantidade)
            se quantidade <= estoque[i].quantidade então
               estoque[i].quantidade <- estoque[i].quantidade - quantidade
               estoque[i].vendas <- estoque[i].vendas + quantidade
               totalVendas <- totalVendas + (estoque[i].preco * quantidade)
               escreva("Venda realizada com sucesso!")
            senao
               escreva("Quantidade insuficiente em estoque!")
            fimse
         senao
            escreva("Produto indisponível em estoque!")
         fimse
         i <- totalProdutos + 1
      fimse
      i <- i + 1
   fimenquanto
   se i > totalProdutos então
      escreva("Produto não encontrado!")
   fimse
fim
```
Neste procedimento, o usuário poderá realizar vendas de produtos. Será solicitado o código do produto a ser vendido. Em seguida, será feita uma busca no vetor "estoque" para encontrar o produto correspondente. Se o produto for encontrado e houver quantidade suficiente em estoque, o usuário será solicitado a digitar a quantidade a ser vendida. Se a quantidade for menor ou igual ao estoque disponível, a quantidade em estoque será atualizada, as vendas serão registradas e o total de vendas será atualizado. Caso contrário, serão exibidas mensagens informando que a quantidade é insuficiente ou que o produto está indisponível. Se o produto não for encontrado, será exibida uma mensagem informando que o produto não foi encontrado.

5. Relatórios:
```
procedimento exibirRelatorios()
inicio
   se totalProdutos > 0 então
      escreva("=== Relatórios ===")
      escreva("Total de produtos cadastrados: ", totalProdutos)
      escreva("Total de vendas realizadas: ", totalVendas)
      escreva("Média de preço dos produtos: ", totalVendas / totalProdutos)
      escreva("Produtos em estoque:")
      para i de 1 ate totalProdutos faça
         escreva("Código: ", estoque[i].codigo)
         escreva("Nome: ", estoque[i].nome)
         escreva("Quantidade: ", estoque[i].quantidade)
         escreva("Preço: ", estoque[i].preco)
         escreva("Vendas: ", estoque[i].vendas)
      fimpara
   senao
      escreva("Nenhum produto cadastrado!")
   fimse
fim
```
Neste procedimento, será exibido um relatório com estatísticas sobre o estoque. Será mostrado o total de produtos cadastrados, o total de vendas realizadas, a média de preço dos produtos e a lista de produtos em estoque, com suas respectivas informações.

6. Programa principal:
```
inicio
   opcao <- 0
   totalProdutos <- 0
   totalVendas <- 0
   enquanto opcao <> 4 faça
      exibirMenu()
      escolha(opcao)
         caso 1
            cadastrarProduto()
         caso 2
            venderProduto()
         caso 3
            exibirRelatorios()
         caso 4
            escreva("Encerrando o programa...")
         outrocaso
            escreva("Opção inválida!")
         fimescolha
   fimenquanto
fim
```
Nesta parte, temos o programa principal. O programa inicializa as variáveis necessárias e entra em um loop que só será interrompido quando o usuário escolher a opção "4" (Sair) no menu. Dentro do loop, será feita a chamada dos procedimentos correspondentes às opções escolhidas pelo usuário.

Este é um exemplo de um código complexo em Portugol que implementa um sistema de gerenciamento de estoque. Claro que você pode adaptá-lo e adicionar mais funcionalidades de acordo com suas necessidades.