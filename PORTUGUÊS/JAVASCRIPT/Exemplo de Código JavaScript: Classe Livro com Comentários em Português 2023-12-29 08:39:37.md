Certamente! Aqui está um exemplo de um código JavaScript complexo, com comentários explicativos em português:

```javascript
// Definindo uma classe chamada Livro
class Livro {
  constructor(titulo, autor, anoPublicacao) {
    this.titulo = titulo;
    this.autor = autor;
    this.anoPublicacao = anoPublicacao;
  }

  // Método para exibir informações do livro
  exibirInformacoes() {
    console.log(`Título: ${this.titulo}`);
    console.log(`Autor: ${this.autor}`);
    console.log(`Ano de Publicação: ${this.anoPublicacao}`);
  }
}

// Criando uma instância da classe Livro
const livro = new Livro("Aprendendo JavaScript", "João da Silva", 2022);

// Chamando o método para exibir informações do livro
livro.exibirInformacoes();
```

Nesse código, é criada uma classe chamada `Livro` que possui um construtor para definir as propriedades `titulo`, `autor` e `anoPublicacao`. Além disso, a classe possui um método chamado `exibirInformacoes`, que mostra os detalhes do livro no console.

Em seguida, é criada uma instância da classe `Livro` chamada `livro` com os valores "Aprendendo JavaScript", "João da Silva" e 2022 para as propriedades. Por fim, é chamado o método `exibirInformacoes()` para mostrar as informações do livro no console.

Esse código pode ser utilizado como base para criar um sistema de gerenciamento de livros, onde seria possível criar, editar e exibir informações sobre diferentes livros de forma organizada.