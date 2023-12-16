Claro! Aqui está um exemplo de um diagrama de classes em UML para um sistema de gerenciamento de biblioteca:

```
@startuml

class Biblioteca {
  - nome: string
  - endereco: string
  - livros: List<Livro>
  - membros: List<Membro>
  + cadastrarLivro(livro: Livro): void
  + cadastrarMembro(membro: Membro): void
  + emprestarLivro(livro: Livro, membro: Membro): boolean
  + devolverLivro(livro: Livro, membro: Membro): boolean
}

class Livro {
  - titulo: string
  - autor: string
  - disponivel: boolean
  + getTitulo(): string
  + getAutor(): string
  + isDisponivel(): boolean
}

class Membro {
  - nome: string
  - cpf: string
  - livrosEmprestados: List<Livro>
  + getNome(): string
  + getCpf(): string
  + getLivrosEmprestados(): List<Livro>
}

Biblioteca "1" -- "n" Livro
Biblioteca "1" -- "n" Membro

@enduml
```

Neste exemplo, temos três classes principais:

1. A classe "Biblioteca" representa a biblioteca em si e possui atributos como nome, endereço, uma lista de livros e uma lista de membros. Além disso, ela possui métodos para cadastrar um livro, cadastrar um membro, emprestar um livro para um membro e devolver um livro.

2. A classe "Livro" representa um livro da biblioteca e possui atributos como título, autor e uma flag indicando se ele está disponível para empréstimo ou não. Ela também possui métodos para obter o título, autor e verificar se o livro está disponível.

3. A classe "Membro" representa um membro da biblioteca e possui atributos como nome, CPF e uma lista de livros emprestados. Ela possui métodos para obter o nome, CPF e a lista de livros emprestados por um membro.

O diagrama de classes mostra também os relacionamentos entre as classes: a classe "Biblioteca" possui uma associação de agregação com as classes "Livro" e "Membro", indicando que a biblioteca possui uma coleção de livros e membros.

Espero que este exemplo atenda às suas necessidades!