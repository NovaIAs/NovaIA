Claro! Aqui está um código complexo em COOL (Classroom Object-Oriented Language) que implementa um sistema de gerenciamento de biblioteca em Português. Vou explicar o código em detalhes:

```
# Classe Livro
class Livro {
  atributo titulo : String;
  atributo autor : String;
  atributo anoPublicacao : Int;
  atributo disponivel : Bool;
  
  # Método construtor
  metodo inicializar(tituloNovo : String, autorNovo : String, anoPublicacaoNovo : Int) : SELF_TYPE {
    {
      titulo <- tituloNovo;
      autor <- autorNovo;
      anoPublicacao <- anoPublicacaoNovo;
      disponivel <- TRUE;
      retornar self;
    }
  }
  
  # Método para emprestar o livro
  metodo emprestar() : Bool {
    se disponivel entao
      disponivel <- FALSE;
      retornar TRUE;
    senao
      retornar FALSE;
    fim
  }
  
  # Método para devolver o livro
  metodo devolver() {
    disponivel <- TRUE;
  }
  
  # Método para exibir informações do livro
  metodo exibirInformacoes() {
    imprimir("Título: " + titulo);
    imprimir("Autor: " + autor);
    imprimir("Ano de Publicação: " + anoPublicacao);
    se disponivel entao
      imprimir("Disponibilidade: Disponível");
    senao
      imprimir("Disponibilidade: Indisponível");
    fim
  }
}

# Classe Biblioteca
class Biblioteca {
  atributo livros : List<Livro>;
  
  # Método construtor
  metodo inicializar() {
    livros <- new List<Livro>;
  }
  
  # Método para adicionar um livro à biblioteca
  metodo adicionarLivro(titulo : String, autor : String, anoPublicacao : Int) {
    livro : Livro <- new Livro;
    livro.inicializar(titulo, autor, anoPublicacao);
    livros.insertBack(livro);
  }
  
  # Método para emprestar um livro da biblioteca
  metodo emprestarLivro(titulo : String) : Bool {
    para cada livro em livros loop
      se livro.titulo = titulo entao
        retornar livro.emprestar();
      fim
    fim
    retornar FALSE;
  }
  
  # Método para devolver um livro à biblioteca
  metodo devolverLivro(titulo : String) {
    para cada livro em livros loop
      se livro.titulo = titulo entao
        livro.devolver();
        parar;
      fim
    fim
  }
  
  # Método para exibir todos os livros da biblioteca
  metodo exibirLivros() {
    se livros.isEmpty() entao
      imprimir("A biblioteca está vazia.");
    senao
      para cada livro em livros loop
        livro.exibirInformacoes();
      fim
    fim
  }
}

# Programa principal
inicial : Biblioteca <- new Biblioteca;
inicial.inicializar();

# Exemplo de utilização
inicial.adicionarLivro("Dom Casmurro", "Machado de Assis", 1899);
inicial.adicionarLivro("Grande Sertão: Veredas", "João Guimarães Rosa", 1956);

inicial.exibirLivros();

inicial.emprestarLivro("Dom Casmurro");
inicial.exibirLivros();

inicial.devolverLivro("Dom Casmurro");
inicial.exibirLivros();
```

Neste código, temos duas classes principais: `Livro` e `Biblioteca`. A classe `Livro` representa os livros individuais, com atributos como título, autor, ano de publicação e disponibilidade. A classe `Biblioteca` representa a coleção de livros e possui métodos para adicionar, emprestar, devolver e exibir os livros.

O programa principal instancia um objeto da classe `Biblioteca` e realiza algumas operações de exemplo, como adicionar livros, emprestar e devolver livros, e exibir informações.

Espero que este código atenda às suas expectativas! Se você tiver mais alguma solicitação, por favor, me avise.