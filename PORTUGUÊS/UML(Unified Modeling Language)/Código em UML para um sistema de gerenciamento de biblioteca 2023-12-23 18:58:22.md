Claro! Aqui está um exemplo de código em UML que representa um sistema de gerenciamento de biblioteca:

```
@startuml

!define DARKCOLORS
!define ENTITYBORDERCOLOR DarkGray
!define ENTITYBACKGROUNDCOLOR White
!define ENTITYTEXTCOLOR Black
!define ENTITYTEXTCOLOR2 Black
!define ENTITYTEXTCOLOR3 Black
!define ENTITYTEXTCOLOR4 Black
!define ENTITYTEXTCOLOR5 Black

skinparam class {
    BackgroundColor White
    BorderColor DarkGray
    ArrowColor DarkGray
    FontColor Black
}

class Biblioteca {
    - nome: String
    - endereco: String
    - telefone: String
    - livros: List<Livro>
    - clientes: List<Cliente>
    + cadastrarLivro(livro: Livro): void
    + removerLivro(livro: Livro): void
    + cadastrarCliente(cliente: Cliente): void
    + removerCliente(cliente: Cliente): void
    + emprestarLivro(cliente: Cliente, livro: Livro): void
    + devolverLivro(cliente: Cliente, livro: Livro): void
    + listarLivrosDisponiveis(): List<Livro>
    + listarLivrosEmprestados(): List<Livro>
    + listarClientes(): List<Cliente>
}

class Livro {
    - titulo: String
    - autor: String
    - anoPublicacao: int
    - disponivel: boolean
    + getTitulo(): String
    + setTitulo(titulo: String): void
    + getAutor(): String
    + setAutor(autor: String): void
    + getAnoPublicacao(): int
    + setAnoPublicacao(ano: int): void
    + isDisponivel(): boolean
    + setDisponivel(disponivel: boolean): void
}

class Cliente {
    - nome: String
    - cpf: String
    - endereco: String
    - telefone: String
    - livrosEmprestados: List<Livro>
    + getNome(): String
    + setNome(nome: String): void
    + getCpf(): String
    + setCpf(cpf: String): void
    + getEndereco(): String
    + setEndereco(endereco: String): void
    + getTelefone(): String
    + setTelefone(telefone: String): void
    + getLivrosEmprestados(): List<Livro>
    + setLivrosEmprestados(livros: List<Livro>): void
}

Biblioteca "1" --> "n" Livro
Biblioteca "1" --> "n" Cliente
Cliente "0..*" --> "0..*" Livro

@enduml
```

Neste código em UML, temos três classes principais: Biblioteca, Livro e Cliente. A classe Biblioteca representa a entidade que gerencia a biblioteca e possui atributos como nome, endereço, telefone, além de listas de livros e clientes. Ela também possui métodos para cadastrar e remover livros e clientes, emprestar e devolver livros, e listar livros disponíveis, livros emprestados e clientes.

A classe Livro representa os livros presentes na biblioteca, com atributos como título, autor, ano de publicação e um indicador de disponibilidade. Além disso, possui métodos para acessar e modificar esses atributos.

A classe Cliente representa os clientes da biblioteca, com atributos como nome, CPF, endereço, telefone e uma lista de livros emprestados. Também possui métodos para acessar e modificar esses atributos.

Essas três classes estão relacionadas entre si por associações, indicando que uma Biblioteca possui vários Livros e vários Clientes, e um Cliente pode ter vários Livros emprestados.

Esse código em UML pode ser utilizado como base para implementar um sistema de gerenciamento de biblioteca em qualquer linguagem de programação.