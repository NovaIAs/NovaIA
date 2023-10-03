Claro! Vou criar um código complexo em Portugol para você. Imagine que estamos desenvolvendo um sistema de gerenciamento de uma biblioteca. Nesse sistema, temos a necessidade de gerenciar os livros disponíveis, os usuários cadastrados e os empréstimos realizados. Vou criar uma estrutura em que o usuário possa realizar empréstimos de livros, devolver os livros emprestados e consultar a disponibilidade de um livro específico. Vamos lá:

```
algoritmo "Sistema de Gerenciamento de Biblioteca"

var
   livros: vetor[1..100] de registro
   usuarios: vetor[1..100] de registro
   emprestimos: vetor[1..100] de registro
   contadorLivros: inteiro
   contadorUsuarios: inteiro
   contadorEmprestimos: inteiro

registro Livro
   titulo: caractere[50]
   autor: caractere[50]
   disponivel: logico

registro Usuario
   nome: caractere[50]
   matricula: caractere[10]

registro Emprestimo
   livro: registro Livro
   usuario: registro Usuario
   dataEmprestimo: caractere[10]
   dataDevolucao: caractere[10]

procedimento cadastrarLivro()
   se contadorLivros < 100 então
      contadorLivros <- contadorLivros + 1
      escreva "Informe o título do livro: "
      leia livros[contadorLivros].titulo
      escreva "Informe o autor do livro: "
      leia livros[contadorLivros].autor
      livros[contadorLivros].disponivel <- verdadeiro
   senão
      escreva "Limite de livros cadastrados atingido!"
   fimse

procedimento cadastrarUsuario()
   se contadorUsuarios < 100 então
      contadorUsuarios <- contadorUsuarios + 1
      escreva "Informe o nome do usuário: "
      leia usuarios[contadorUsuarios].nome
      escreva "Informe a matrícula do usuário: "
      leia usuarios[contadorUsuarios].matricula
   senão
      escreva "Limite de usuários cadastrados atingido!"
   fimse

procedimento realizarEmprestimo()
   escreva "Informe o número do livro que deseja emprestar: "
   leia numLivro
   escreva "Informe a matrícula do usuário: "
   leia matriculaUsuario
   se numLivro > 0 e numLivro <= contadorLivros e matriculaUsuario > 0 e matriculaUsuario <= contadorUsuarios então
      se livros[numLivro].disponivel então
         contadorEmprestimos <- contadorEmprestimos + 1
         emprestimos[contadorEmprestimos].livro <- livros[numLivro]
         emprestimos[contadorEmprestimos].usuario <- usuarios[matriculaUsuario]
         escreva "Informe a data do empréstimo: "
         leia emprestimos[contadorEmprestimos].dataEmprestimo
         escreva "Informe a data de devolução: "
         leia emprestimos[contadorEmprestimos].dataDevolucao
         livros[numLivro].disponivel <- falso
         escreva "Empréstimo realizado com sucesso!"
      senão
         escreva "Livro indisponível para empréstimo!"
      fimse
   senão
      escreva "Livro ou usuário não encontrado!"
   fimse

procedimento realizarDevolucao()
   escreva "Informe o número do empréstimo que deseja devolver: "
   leia numEmprestimo
   se numEmprestimo > 0 e numEmprestimo <= contadorEmprestimos então
      numLivro <- 1
      encontrou <- falso
      enquanto numLivro <= contadorLivros e não encontrou faça
         se emprestimos[numEmprestimo].livro = livros[numLivro] então
            encontrou <- verdadeiro
            livros[numLivro].disponivel <- verdadeiro
         senão
            numLivro <- numLivro + 1
         fimse
      fimenquanto
      se encontrou então
         escreva "Devolução realizada com sucesso!"
      senão
         escreva "Empréstimo não encontrado!"
      fimse
   senão
      escreva "Empréstimo inválido!"
   fimse

procedimento consultarDisponibilidade()
   escreva "Informe o título do livro que deseja consultar: "
   leia tituloLivro
   numLivro <- 1
   encontrou <- falso
   enquanto numLivro <= contadorLivros e não encontrou faça
      se livros[numLivro].titulo = tituloLivro então
         encontrou <- verdadeiro
         se livros[numLivro].disponivel então
            escreva "O livro ", livros[numLivro].titulo, " está disponível."
         senão
            escreva "O livro ", livros[numLivro].titulo, " está indisponível."
         fimse
      fimse
      numLivro <- numLivro + 1
   fimenquanto
   se not encontrou então
      escreva "Livro não encontrado!"
   fimse

funcao menu() : caractere
   escreva "-------- MENU --------"
   escreva "1. Cadastrar livro"
   escreva "2. Cadastrar usuário"
   escreva "3. Realizar empréstimo"
   escreva "4. Realizar devolução"
   escreva "5. Consultar disponibilidade de livro"
   escreva "6. Sair"
   escreva "Digite a opção desejada: "
   leia opcao
   retorne opcao

// Programa principal
contadorLivros <- 0
contadorUsuarios <- 0
contadorEmprestimos <- 0
opcao <- ""
enquanto opcao <> "6" faça
   opcao <- menu()
   escolha opcao
      caso "1"
         cadastrarLivro()
      caso "2"
         cadastrarUsuario()
      caso "3"
         realizarEmprestimo()
      caso "4"
         realizarDevolucao()
      caso "5"
         consultarDisponibilidade()
      caso "6"
         escreva "Saindo do sistema..."
      outrocaso
         escreva "Opção inválida!"
   fimescolha
fimenquanto
```

Neste código, utilizamos vetores de registros para armazenar as informações dos livros, usuários e empréstimos. Cada registro possui campos específicos para armazenar os dados necessários. Os procedimentos `cadastrarLivro`, `cadastrarUsuario`, `realizarEmprestimo`, `realizarDevolucao` e `consultarDisponibilidade` são responsáveis por solicitar os dados do usuário e realizar as operações correspondentes. A função `menu` exibe as opções disponíveis para o usuário e retorna a opção selecionada.

Espero que este código atenda às suas expectativas. Lembre-se de adaptá-lo conforme necessário para o ambiente em que deseja executá-lo.