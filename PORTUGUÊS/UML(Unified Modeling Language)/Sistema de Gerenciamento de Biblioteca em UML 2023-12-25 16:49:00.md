Certamente! Aqui está um código complexo em UML que representa um sistema de gerenciamento de biblioteca:

```plantuml
@startuml
class Biblioteca {
  - nome: String
  - endereco: String
  - listaLivros: List<Livro>
  - listaUsuarios: List<Usuario>
  - listaEmprestimos: List<Emprestimo>

  + adicionarLivro(livro: Livro): void
  + removerLivro(livro: Livro): void
  + adicionarUsuario(usuario: Usuario): void
  + removerUsuario(usuario: Usuario): void
  + emprestarLivro(livro: Livro, usuario: Usuario): void
  + devolverLivro(livro: Livro, usuario: Usuario): void
  + pesquisarLivro(titulo: String): List<Livro>
  + pesquisarLivroPorAutor(autor: String): List<Livro>
  + pesquisarLivroPorGenero(genero: String): List<Livro>
  + listarLivrosDisponiveis(): List<Livro>
  + listarLivrosEmprestados(): List<Livro>
  + listarLivrosAtrasados(): List<Livro>
}

class Livro {
  - codigo: String
  - titulo: String
  - autor: String
  - genero: String
  - disponivel: boolean

  + getCodigo(): String
  + getTitulo(): String
  + getAutor(): String
  + getGenero(): String
  + isDisponivel(): boolean
}

class Usuario {
  - codigo: String
  - nome: String
  - email: String
  - telefone: String

  + getCodigo(): String
  + getNome(): String
  + getEmail(): String
  + getTelefone(): String
}

class Emprestimo {
  - livro: Livro
  - usuario: Usuario
  - dataEmprestimo: Date
  - dataDevolucao: Date

  + getLivro(): Livro
  + getUsuario(): Usuario
  + getDataEmprestimo(): Date
  + getDataDevolucao(): Date
}

Biblioteca "1" --> "*" Livro
Biblioteca "1" --> "*" Usuario
Biblioteca "1" --> "*" Emprestimo
@enduml
```

Neste código, temos as classes `Biblioteca`, `Livro`, `Usuario` e `Emprestimo`. A classe `Biblioteca` representa uma biblioteca real e possui atributos como `nome`, `endereco` e listas de `livros`, `usuarios` e `emprestimos`.

A classe `Livro` representa um livro presente na biblioteca, com atributos como `codigo`, `titulo`, `autor`, `genero` e um indicador de disponibilidade.

A classe `Usuario` representa um usuário que pode fazer empréstimo de livros da biblioteca, com atributos como `codigo`, `nome`, `email` e `telefone`.

A classe `Emprestimo` representa um empréstimo realizado por um usuário, relacionando um `livro` e um `usuario`, além de armazenar as datas de empréstimo e devolução.

A biblioteca possui métodos para adicionar e remover livros e usuários, emprestar e devolver livros, pesquisar livros por título, autor e gênero, e listar livros disponíveis, emprestados e atrasados.

Essa é uma representação básica de um sistema de gerenciamento de biblioteca em UML, com suas classes, atributos e relacionamentos.