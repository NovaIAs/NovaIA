Claro! Aqui está um exemplo de um código em UML que representa um sistema de gerenciamento de biblioteca:

```
@startuml

class Biblioteca {
  - nome: String
  - endereco: String
  - livros: ArrayList<Livro>

  + adicionarLivro(livro: Livro): void
  + removerLivro(livro: Livro): void
  + buscarLivroPorTitulo(titulo: String): Livro
  + buscarLivroPorAutor(autor: String): List<Livro>
}

class Livro {
  - titulo: String
  - autor: String
  - genero: String
  - disponivel: boolean

  + getTitulo(): String
  + getAutor(): String
  + getGenero(): String
  + isDisponivel(): boolean
  + emprestar(): void
  + devolver(): void
}

class Pessoa {
  - nome: String
  - endereco: String
  - telefone: String

  + getNome(): String
  + getEndereco(): String
  + getTelefone(): String
}

class Usuario {
  - pessoa: Pessoa
  - livrosEmprestados: ArrayList<Livro>

  + getPessoa(): Pessoa
  + getLivrosEmprestados(): ArrayList<Livro>
  + emprestarLivro(livro: Livro): void
  + devolverLivro(livro: Livro): void
}

Biblioteca "1" *-- "*" Livro
Usuario "1" -- "1" Pessoa
Usuario "0..*" *-- "*" Livro

@enduml

```

Neste exemplo, temos três classes principais: Biblioteca, Livro e Pessoa. A classe Biblioteca representa uma biblioteca, contendo informações como nome, endereço e uma lista de livros disponíveis. A classe Livro representa um livro, com atributos como título, autor, gênero e uma flag indicando se está disponível para empréstimo. A classe Pessoa representa uma pessoa, com informações como nome, endereço e telefone.

Além disso, temos a classe Usuario, que possui uma associação de cardinalidade 1 para 1 com a classe Pessoa, indicando que um usuário está associado a uma pessoa específica. A classe Usuario também possui uma associação de cardinalidade 0 ou mais para * com a classe Livro, indicando que um usuário pode emprestar vários livros.

Essas classes e suas associações podem ser usadas como base para implementar um sistema de gerenciamento de biblioteca, onde é possível adicionar livros à biblioteca, emprestar e devolver livros, buscar livros por título e autor, entre outras funcionalidades. O código em UML fornece uma representação visual das classes e suas relações, facilitando o entendimento e a implementação do sistema.