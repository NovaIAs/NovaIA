Claro! Vou criar um código complexo em Portugol para você. Este código é um sistema de gerenciamento de vendas em uma loja fictícia. Ele permite adicionar, buscar, editar e excluir produtos, além de calcular o total de vendas e exibir relatórios. Vamos lá:

```
algoritmo Loja
    // Declaração de variáveis
    var
        produtos: vetor[1..100] de registro
            codigo: inteiro
            nome: caractere
            preco: real
        totalVendas: real

    // Função para adicionar um produto
    funcao adicionarProduto()        
        escreva("Informe o código do produto:")
        leia(produtos[codigo])
        escreva("Informe o nome do produto:")
        leia(produtos[nome])
        escreva("Informe o preço do produto:")
        leia(produtos[preco])

    // Função para buscar um produto
    funcao buscarProduto(cod: inteiro): inteiro
        para i de 1 ate 100 faca
            se produtos[i].codigo = cod entao
                escreva("Produto encontrado:")
                escreva("Código: ", produtos[i].codigo)
                escreva("Nome: ", produtos[i].nome)
                escreva("Preço: ", produtos[i].preco)
                retorne i
        fimpara
        retorne -1

    // Função para editar um produto
    funcao editarProduto(cod: inteiro)
        var
            posicao: inteiro
        posicao <- buscarProduto(cod)
        se posicao <> -1 entao
            escreva("Informe o novo nome do produto:")
            leia(produtos[posicao].nome)
            escreva("Informe o novo preço do produto:")
            leia(produtos[posicao].preco)
            escreva("Produto editado com sucesso!")
        senao
            escreva("Produto não encontrado!")
            
    // Função para excluir um produto
    funcao excluirProduto(cod: inteiro)
        var
            posicao: inteiro
        posicao <- buscarProduto(cod)
        se posicao <> -1 entao
            produtos[posicao] <- produtos[100]
            escreva("Produto excluído com sucesso!")
        senao
            escreva("Produto não encontrado!")

    // Função para calcular o total de vendas
    funcao calcularTotalVendas()
        totalVendas <- 0
        para i de 1 ate 100 faca
            totalVendas <- totalVendas + produtos[i].preco
        fimpara

    // Função para exibir relatório de vendas
    funcao exibirRelatorioVendas()
        para i de 1 ate 100 faca
            se produtos[i].codigo <> 0 entao
                escreva("Código: ", produtos[i].codigo)
                escreva("Nome: ", produtos[i].nome)
                escreva("Preço: ", produtos[i].preco)
        fimpara

    // Programa principal
    inicio
        var
            opcao: caractere
            codigo: inteiro
        repita
            escreva("------ Menu ------")
            escreva("1. Adicionar produto")
            escreva("2. Buscar produto")
            escreva("3. Editar produto")
            escreva("4. Excluir produto")
            escreva("5. Calcular total de vendas")
            escreva("6. Exibir relatório de vendas")
            escreva("0. Sair")
            escreva("-------------------")
            escreva("Digite a opção desejada:")
            leia(opcao)

            escolha opcao
                caso "1"
                    adicionarProduto()
                caso "2"
                    escreva("Informe o código do produto:")
                    leia(codigo)
                    buscarProduto(codigo)
                caso "3"
                    escreva("Informe o código do produto:")
                    leia(codigo)
                    editarProduto(codigo)
                caso "4"
                    escreva("Informe o código do produto:")
                    leia(codigo)
                    excluirProduto(codigo)
                caso "5"
                    calcularTotalVendas()
                    escreva("Total de vendas: ", totalVendas)
                caso "6"
                    exibirRelatorioVendas()
                caso "0"
                    escreva("Saindo do programa...")
                caso contrario
                    escreva("Opção inválida!")
            fimescolha
        ate opcao = "0"
    fim
fimalgoritmo
```

Este código é bastante complexo e envolve várias funcionalidades. Ele utiliza um vetor de registros para armazenar os produtos, com campos para código, nome e preço. As funções permitem adicionar, buscar, editar e excluir produtos, enquanto outras funções calculam o total de vendas e exibem relatórios. O programa principal apresenta um menu onde o usuário pode escolher as opções desejadas.

Espero que este código atenda às suas expectativas! Se tiver alguma dúvida ou precisar de mais ajuda, é só me dizer.