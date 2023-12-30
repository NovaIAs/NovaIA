Claro, vou gerar um código complexo em Portugol para você. Este código é um exemplo de um sistema de gerenciamento de uma biblioteca. Ele permite cadastrar livros, cadastrar usuários, emprestar e devolver livros, além de consultar informações sobre os livros e usuários cadastrados. 

```
algoritmo Biblioteca

funcao cadastrarLivro(livros: vetor[1..100] de registro; quantidadeLivros: inteiro): inteiro
    se quantidadeLivros >= 100 entao
        escreva("Não é possível cadastrar mais livros!")
        retorne quantidadeLivros
    fimse
    
    escreva("Digite o nome do livro:")
    leia(livros[quantidadeLivros + 1].nome)
    
    escreva("Digite o autor do livro:")
    leia(livros[quantidadeLivros + 1].autor)
    
    escreva("Digite o ano de publicação do livro:")
    leia(livros[quantidadeLivros + 1].anoPublicacao)
    
    quantidadeLivros <- quantidadeLivros + 1
    
    retorne quantidadeLivros
fimfuncao

funcao cadastrarUsuario(usuarios: vetor[1..100] de registro; quantidadeUsuarios: inteiro): inteiro
    se quantidadeUsuarios >= 100 entao
        escreva("Não é possível cadastrar mais usuários!")
        retorne quantidadeUsuarios
    fimse
    
    escreva("Digite o nome do usuário:")
    leia(usuarios[quantidadeUsuarios + 1].nome)
    
    escreva("Digite o telefone do usuário:")
    leia(usuarios[quantidadeUsuarios + 1].telefone)
    
    quantidadeUsuarios <- quantidadeUsuarios + 1
    
    retorne quantidadeUsuarios
fimfuncao

procedimento emprestarLivro(livros: vetor[1..100] de registro; quantidadeLivros: inteiro; usuarios: vetor[1..100] de registro; quantidadeUsuarios: inteiro)
    escreva("Digite o nome do livro a ser emprestado:")
    leia(nomeLivro)
    
    indiceLivro <- -1
    para i de 1 ate quantidadeLivros faca
        se livros[i].nome = nomeLivro entao
            indiceLivro <- i
            saia
        fimse
    fimpara
    
    se indiceLivro = -1 entao
        escreva("Livro não encontrado!")
    senao
        escreva("Digite o nome do usuário que irá emprestar o livro:")
        leia(nomeUsuario)
        
        indiceUsuario <- -1
        para i de 1 ate quantidadeUsuarios faca
            se usuarios[i].nome = nomeUsuario entao
                indiceUsuario <- i
                saia
            fimse
        fimpara
        
        se indiceUsuario = -1 entao
            escreva("Usuário não encontrado!")
        senao
            se livros[indiceLivro].emprestado entao
                escreva("Livro já está emprestado!")
            senao
                livros[indiceLivro].emprestado <- verdadeiro
                livros[indiceLivro].usuarioEmprestimo <- usuarios[indiceUsuario].nome
                escreva("Livro emprestado com sucesso para ", usuarios[indiceUsuario].nome)
            fimse
        fimse
    fimse
fimprocedimento

procedimento devolverLivro(livros: vetor[1..100] de registro; quantidadeLivros: inteiro)
    escreva("Digite o nome do livro a ser devolvido:")
    leia(nomeLivro)
    
    indiceLivro <- -1
    para i de 1 ate quantidadeLivros faca
        se livros[i].nome = nomeLivro entao
            indiceLivro <- i
            saia
        fimse
    fimpara
    
    se indiceLivro = -1 entao
        escreva("Livro não encontrado!")
    senao
        se livros[indiceLivro].emprestado entao
            escreva("Digite o nome do usuário que está devolvendo o livro:")
            leia(nomeUsuario)
            
            se livros[indiceLivro].usuarioEmprestimo = nomeUsuario entao
                livros[indiceLivro].emprestado <- falso
                livros[indiceLivro].usuarioEmprestimo <- ""
                escreva("Livro devolvido com sucesso!")
            senao
                escreva("Este livro não foi emprestado para ", nomeUsuario)
            fimse
        senao
            escreva("Este livro não está emprestado!")
        fimse
    fimse
fimprocedimento

funcao consultarLivros(livros: vetor[1..100] de registro; quantidadeLivros: inteiro)
    para i de 1 ate quantidadeLivros faca
        escreva("Nome do livro: ", livros[i].nome)
        escreva("Autor: ", livros[i].autor)
        escreva("Ano de publicação: ", livros[i].anoPublicacao)
        se livros[i].emprestado entao
            escreva("Livro emprestado para: ", livros[i].usuarioEmprestimo)
        senao
            escreva("Livro disponível para empréstimo")
        fimse
        escreva("---------------------------")
    fimpara
fimfuncao

funcao consultarUsuarios(usuarios: vetor[1..100] de registro; quantidadeUsuarios: inteiro)
    para i de 1 ate quantidadeUsuarios faca
        escreva("Nome do usuário: ", usuarios[i].nome)
        escreva("Telefone: ", usuarios[i].telefone)
        escreva("---------------------------")
    fimpara
fimfuncao

var
    livros: vetor[1..100] de registro
    usuarios: vetor[1..100] de registro
    quantidadeLivros, quantidadeUsuarios: inteiro
    opcao: caractere
    
quantidadeLivros <- 0
quantidadeUsuarios <- 0

repita
    escreva("----------------------------------------")
    escreva("Biblioteca - Sistema de Gerenciamento")
    escreva("----------------------------------------")
    escreva("Selecione uma opção:")
    escreva("1 - Cadastrar livro")
    escreva("2 - Cadastrar usuário")
    escreva("3 - Emprestar livro")
    escreva("4 - Devolver livro")
    escreva("5 - Consultar livros")
    escreva("6 - Consultar usuários")
    escreva("0 - Sair")
    leia(opcao)
    
    escolha opcao
        caso "1"
            quantidadeLivros <- cadastrarLivro(livros, quantidadeLivros)
        caso "2"
            quantidadeUsuarios <- cadastrarUsuario(usuarios, quantidadeUsuarios)
        caso "3"
            emprestarLivro(livros, quantidadeLivros, usuarios, quantidadeUsuarios)
        caso "4"
            devolverLivro(livros, quantidadeLivros)
        caso "5"
            consultarLivros(livros, quantidadeLivros)
        caso "6"
            consultarUsuarios(usuarios, quantidadeUsuarios)
        caso "0"
            escreva("Encerrando o programa...")
        caso contrario
            escreva("Opção inválida!")
    fimescolha
ate opcao <> "0"

fimalgoritmo
```

Este código em Portugol é um exemplo de um sistema de gerenciamento de biblioteca básico. Ele inclui funções para cadastrar livros e usuários, emprestar e devolver livros, além de consultar informações sobre os livros e usuários cadastrados. O código utiliza estruturas de repetição, vetores e registros para armazenar os dados.