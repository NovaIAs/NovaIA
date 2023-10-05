Claro! Aqui está um exemplo de código complexo em TypeScript que demonstra um sistema de gerenciamento de biblioteca:

```typescript
class Livro {
  private titulo: string;
  private autor: string;
  private anoPublicacao: number;
  
  constructor(titulo: string, autor: string, anoPublicacao: number) {
    this.titulo = titulo;
    this.autor = autor;
    this.anoPublicacao = anoPublicacao;
  }
  
  public getTitulo(): string {
    return this.titulo;
  }
  
  public getAutor(): string {
    return this.autor;
  }
  
  public getAnoPublicacao(): number {
    return this.anoPublicacao;
  }
}

class Biblioteca {
  private livros: Livro[];
  
  constructor() {
    this.livros = [];
  }
  
  public adicionarLivro(livro: Livro): void {
    this.livros.push(livro);
  }
  
  public buscarLivroPorTitulo(titulo: string): Livro | undefined {
    return this.livros.find(livro => livro.getTitulo() === titulo);
  }
  
  public buscarLivrosPorAutor(autor: string): Livro[] {
    return this.livros.filter(livro => livro.getAutor() === autor);
  }
  
  public buscarLivrosPorAno(ano: number): Livro[] {
    return this.livros.filter(livro => livro.getAnoPublicacao() === ano);
  }
  
  public removerLivro(titulo: string): void {
    this.livros = this.livros.filter(livro => livro.getTitulo() !== titulo);
  }
}

// Exemplo de uso do código:

const biblioteca = new Biblioteca();

const livro1 = new Livro("Dom Casmurro", "Machado de Assis", 1899);
const livro2 = new Livro("O Senhor dos Anéis", "J.R.R. Tolkien", 1954);
const livro3 = new Livro("A Revolução dos Bichos", "George Orwell", 1945);

biblioteca.adicionarLivro(livro1);
biblioteca.adicionarLivro(livro2);
biblioteca.adicionarLivro(livro3);

const livroEncontrado = biblioteca.buscarLivroPorTitulo("Dom Casmurro");
console.log(livroEncontrado); // Livro { titulo: 'Dom Casmurro', autor: 'Machado de Assis', anoPublicacao: 1899 }

const livrosDoAutor = biblioteca.buscarLivrosPorAutor("J.R.R. Tolkien");
console.log(livrosDoAutor); // [ Livro { titulo: 'O Senhor dos Anéis', autor: 'J.R.R. Tolkien', anoPublicacao: 1954 } ]

const livrosDoAno = biblioteca.buscarLivrosPorAno(1945);
console.log(livrosDoAno); // [ Livro { titulo: 'A Revolução dos Bichos', autor: 'George Orwell', anoPublicacao: 1945 } ]

biblioteca.removerLivro("Dom Casmurro");
console.log(biblioteca); // Biblioteca { livros: [ Livro { titulo: 'O Senhor dos Anéis', autor: 'J.R.R. Tolkien', anoPublicacao: 1954 }, Livro { titulo: 'A Revolução dos Bichos', autor: 'George Orwell', anoPublicacao: 1945 } ] }
```

Neste exemplo, temos duas classes: `Livro` e `Biblioteca`. A classe `Livro` representa um livro com propriedades como título, autor e ano de publicação. A classe `Biblioteca` representa uma biblioteca que possui uma lista de livros e métodos para adicionar, buscar e remover livros.

Você pode criar instâncias da classe `Livro` e adicioná-las à `Biblioteca`. Em seguida, é possível fazer buscas por título, autor ou ano de publicação, e também remover livros da biblioteca.

Espero que este exemplo atenda às suas expectativas de complexidade e originalidade! Se você precisar de mais ajuda ou de algum outro código, estou aqui para ajudar.