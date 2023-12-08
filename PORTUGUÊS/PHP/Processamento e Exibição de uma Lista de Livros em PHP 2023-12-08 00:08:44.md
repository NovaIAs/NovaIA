Claro! Vou criar um código complexo em PHP que envolve o processamento e exibição de uma lista de livros. Ele terá várias funcionalidades, desde a adição e remoção de livros, até a busca e classificação por diferentes critérios. Vou explicar cada parte abaixo:

```php
<?php

class Livro {
  private $titulo;
  private $autor;
  private $anoPublicacao;

  public function __construct($titulo, $autor, $anoPublicacao) {
    $this->titulo = $titulo;
    $this->autor = $autor;
    $this->anoPublicacao = $anoPublicacao;
  }

  public function getTitulo() {
    return $this->titulo;
  }

  public function getAutor() {
    return $this->autor;
  }

  public function getAnoPublicacao() {
    return $this->anoPublicacao;
  }
}

class Biblioteca {
  private $livros;

  public function __construct() {
    $this->livros = array();
  }

  public function adicionarLivro($titulo, $autor, $anoPublicacao) {
    $livro = new Livro($titulo, $autor, $anoPublicacao);
    $this->livros[] = $livro;
  }

  public function removerLivro($titulo) {
    foreach ($this->livros as $indice => $livro) {
      if ($livro->getTitulo() == $titulo) {
        unset($this->livros[$indice]);
        break;
      }
    }
  }

  public function buscarLivroPorAutor($autor) {
    $livrosEncontrados = array();
    foreach ($this->livros as $livro) {
      if ($livro->getAutor() == $autor) {
        $livrosEncontrados[] = $livro;
      }
    }
    return $livrosEncontrados;
  }

  public function ordernarLivrosPorAnoPublicacao() {
    usort($this->livros, function ($livro1, $livro2) {
      return $livro1->getAnoPublicacao() - $livro2->getAnoPublicacao();
    });
  }

  public function exibirLivros() {
    foreach ($this->livros as $livro) {
      echo "Título: " . $livro->getTitulo() . "\n";
      echo "Autor: " . $livro->getAutor() . "\n";
      echo "Ano de Publicação: " . $livro->getAnoPublicacao() . "\n";
      echo "\n";
    }
  }
}

// Exemplo de uso

$biblioteca = new Biblioteca();

$biblioteca->adicionarLivro("Dom Casmurro", "Machado de Assis", 1899);
$biblioteca->adicionarLivro("1984", "George Orwell", 1949);
$biblioteca->adicionarLivro("Harry Potter e a Pedra Filosofal", "J.K. Rowling", 1997);
$biblioteca->adicionarLivro("O Senhor dos Anéis: A Sociedade do Anel", "J.R.R. Tolkien", 1954);
$biblioteca->adicionarLivro("Cem Anos de Solidão", "Gabriel García Márquez", 1967);

$biblioteca->removerLivro("1984");

$livrosDoAutor = $biblioteca->buscarLivroPorAutor("Machado de Assis");
echo "Livros de Machado de Assis:\n";
foreach ($livrosDoAutor as $livro) {
  echo "Título: " . $livro->getTitulo() . "\n";
  echo "Ano de Publicação: " . $livro->getAnoPublicacao() . "\n";
  echo "\n";
}

$biblioteca->ordernarLivrosPorAnoPublicacao();
echo "Livros ordenados por ano de publicação:\n";
$biblioteca->exibirLivros();

?>
```

Explicação:
- Neste código, temos duas classes principais: "Livro" e "Biblioteca".
- A classe "Livro" tem três propriedades privadas (titulo, autor e anoPublicacao) e um construtor que recebe esses valores como parâmetros. Também possui os métodos getters para cada uma das propriedades.
- A classe "Biblioteca" possui uma propriedade privada (livros), que é um array para armazenar os objetos "Livro".
- O método "adicionarLivro" da classe "Biblioteca" cria um novo objeto "Livro" com os parâmetros fornecidos e o adiciona ao array "livros".
- O método "removerLivro" percorre o array "livros" e verifica se o título do livro coincide com o título fornecido. Caso positivo, o livro é removido utilizando a função "unset".
- O método "buscarLivroPorAutor" percorre o array "livros" e verifica se o autor do livro coincide com o autor fornecido. Os livros encontrados são armazenados em um novo array e retornados.
- O método "ordernarLivrosPorAnoPublicacao" utiliza a função "usort" para ordenar os livros do array "livros" com base no ano de publicação.
- O método "exibirLivros" percorre o array "livros" e exibe informações sobre cada livro (título, autor e ano de publicação).
- No exemplo de uso, é criada uma nova instância da classe "Biblioteca", e são adicionados diferentes livros utilizando o método "adicionarLivro". Em seguida, um livro é removido com o método "removerLivro".
- É feita uma busca por livros do autor "Machado de Assis" utilizando o método "buscarLivroPorAutor" e os resultados são exibidos.
- Os livros da biblioteca são ordenados por ano de publicação com o método "ordernarLivrosPorAnoPublicacao" e, em seguida, são exibidos com o método "exibirLivros".

Espero que este código complexo atenda às suas expectativas! Lembre-se de que este é apenas um exemplo e você pode modificar e personalizar o código conforme suas necessidades.