Claro! Vou criar um código complexo em Portugol para você. O código será um programa de gerenciamento de estoque de uma loja. Ele terá várias funcionalidades, como adicionar produtos ao estoque, remover produtos do estoque, listar os produtos disponíveis e atualizar a quantidade de um produto específico. 

Aqui está o código:

```
algoritmo GerenciamentoEstoque

var
   estoque: vetor[1..100] de registro
   opcao: inteiro
   codigo: inteiro
   nome: caractere[50]
   quantidade: inteiro
   i: inteiro

procedimento adicionarProduto()
inicio
   escreva("Digite o código do produto: ")
   leia(codigo)
   escreva("Digite o nome do produto: ")
   leia(nome)
   escreva("Digite a quantidade do produto: ")
   leia(quantidade)
   
   // Procura a próxima posição disponível no vetor
   i <- 1
   enquanto (i <= 100 E estoque[i].codigo <> 0) faca
      i <- i + 1
   fim_enquanto
   
   // Verifica se encontrou uma posição disponível
   se (i <= 100) entao
      estoque[i].codigo <- codigo
      estoque[i].nome <- nome
      estoque[i].quantidade <- quantidade
      escreva("Produto adicionado com sucesso!")
   senao
      escreva("Não foi possível adicionar o produto. Estoque cheio!")
   fim_se
fim_procedimento

procedimento removerProduto()
inicio
   escreva("Digite o código do produto que deseja remover: ")
   leia(codigo)
   
   // Procura o produto no estoque pelo código
   i <- 1
   enquanto (i <= 100 E estoque[i].codigo <> codigo) faca
      i <- i + 1
   fim_enquanto
   
   // Verifica se encontrou o produto
   se (i <= 100) entao
      // Desloca os produtos seguintes para a posição anterior
      para j <- i ate 99 faca
         estoque[j].codigo <- estoque[j+1].codigo
         estoque[j].nome <- estoque[j+1].nome
         estoque[j].quantidade <- estoque[j+1].quantidade
      fim_para
      
      estoque[100].codigo <- 0
      estoque[100].nome <- ""
      estoque[100].quantidade <- 0
      
      escreva("Produto removido com sucesso!")
   senao
      escreva("Produto não encontrado!")
   fim_se
fim_procedimento

procedimento listarProdutos()
inicio
   escreva("Lista de produtos:")
   
   // Percorre o estoque e exibe os produtos cadastrados
   para i <- 1 ate 100 faca
      se (estoque[i].codigo <> 0) entao
         escreva("Código: ", estoque[i].codigo)
         escreva("Nome: ", estoque[i].nome)
         escreva("Quantidade: ", estoque[i].quantidade)
      fim_se
   fim_para
fim_procedimento

procedimento atualizarQuantidade()
inicio
   escreva("Digite o código do produto que deseja atualizar: ")
   leia(codigo)
   
   // Procura o produto no estoque pelo código
   i <- 1
   enquanto (i <= 100 E estoque[i].codigo <> codigo) faca
      i <- i + 1
   fim_enquanto
   
   // Verifica se encontrou o produto
   se (i <= 100) entao
      escreva("Digite a nova quantidade do produto: ")
      leia(quantidade)
      estoque[i].quantidade <- quantidade
      escreva("Quantidade atualizada com sucesso!")
   senao
      escreva("Produto não encontrado!")
   fim_se
fim_procedimento

// Programa principal
inicio
   // Inicializa o estoque
   para i <- 1 ate 100 faca
      estoque[i].codigo <- 0
      estoque[i].nome <- ""
      estoque[i].quantidade <- 0
   fim_para
   
   repita
      escreva("----- MENU -----")
      escreva("1. Adicionar produto")
      escreva("2. Remover produto")
      escreva("3. Listar produtos")
      escreva("4. Atualizar quantidade")
      escreva("5. Sair")
      escreva("-----------------")
      escreva("Digite a opção desejada: ")
      leia(opcao)
      
      escolha opcao
         caso 1:
            adicionarProduto()
         caso 2:
            removerProduto()
         caso 3:
            listarProdutos()
         caso 4:
            atualizarQuantidade()
         caso 5:
            escreva("Programa encerrado.")
         outrocaso:
            escreva("Opção inválida!")
      fim_escolha
   enquanto (opcao <> 5)
fim
```

Esse código cria um menu interativo onde o usuário pode escolher diferentes opções para gerenciar o estoque da loja. Ele utiliza um vetor de registros para armazenar as informações dos produtos, como código, nome e quantidade. As funcionalidades são implementadas em procedimentos separados e são chamadas de acordo com a opção selecionada pelo usuário.

Espero que esse código atenda às suas necessidades! Caso precise de mais alguma coisa, estou à disposição.