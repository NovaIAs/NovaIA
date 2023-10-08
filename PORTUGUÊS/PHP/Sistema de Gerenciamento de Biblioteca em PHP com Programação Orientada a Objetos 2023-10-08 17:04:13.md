Claro! Aqui está um código complexo em PHP que utiliza programação orientada a objetos para criar um sistema de gerenciamento de biblioteca.

```php
<?php

// Classe Book representa um livro
class Book {
    private $title;
    private $author;
    private $year;
    private $available;

    public function __construct($title, $author, $year) {
        $this->title = $title;
        $this->author = $author;
        $this->year = $year;
        $this->available = true;
    }

    public function getTitle() {
        return $this->title;
    }

    public function getAuthor() {
        return $this->author;
    }

    public function getYear() {
        return $this->year;
    }

    public function isAvailable() {
        return $this->available;
    }

    public function borrow() {
        if ($this->available) {
            $this->available = false;
            echo "Livro '{$this->title}' foi emprestado com sucesso.\n";
        } else {
            echo "Livro '{$this->title}' não está disponível para empréstimo.\n";
        }
    }

    public function returnBook() {
        if (!$this->available) {
            $this->available = true;
            echo "Livro '{$this->title}' foi devolvido.\n";
        } else {
            echo "Livro '{$this->title}' já está disponível.\n";
        }
    }
}

// Classe Library representa uma biblioteca
class Library {
    private $books;

    public function __construct() {
        $this->books = [];
    }

    public function addBook(Book $book) {
        $this->books[] = $book;
        echo "Livro '{$book->getTitle()}' adicionado à biblioteca.\n";
    }

    public function listBooks() {
        foreach ($this->books as $book) {
            $status = $book->isAvailable() ? 'Disponível' : 'Indisponível';
            echo "Título: {$book->getTitle()}, Autor: {$book->getAuthor()}, Ano: {$book->getYear()}, Status: $status\n";
        }
    }

    public function borrowBook($title) {
        foreach ($this->books as $book) {
            if ($book->getTitle() === $title) {
                $book->borrow();
                return;
            }
        }
        echo "Livro '{$title}' não encontrado na biblioteca.\n";
    }

    public function returnBook($title) {
        foreach ($this->books as $book) {
            if ($book->getTitle() === $title) {
                $book->returnBook();
                return;
            }
        }
        echo "Livro '{$title}' não encontrado na biblioteca.\n";
    }
}

// Cria uma nova biblioteca
$library = new Library();

// Adiciona alguns livros à biblioteca
$library->addBook(new Book("Dom Casmurro", "Machado de Assis", 1899));
$library->addBook(new Book("1984", "George Orwell", 1949));
$library->addBook(new Book("O Senhor dos Anéis", "J.R.R. Tolkien", 1954));

// Lista todos os livros da biblioteca
$library->listBooks();

// Empréstimo de um livro
$library->borrowBook("1984");

// Tenta emprestar um livro indisponível
$library->borrowBook("Dom Casmurro");

// Devolução de um livro
$library->returnBook("1984");

// Lista todos os livros da biblioteca novamente
$library->listBooks();

?>
```

Este código em PHP cria duas classes: `Book` (Livro) e `Library` (Biblioteca). A classe `Book` representa um livro com propriedades como título, autor, ano e disponibilidade. Ela possui métodos para verificar se o livro está disponível, emprestar e devolver o livro.

A classe `Library` representa uma biblioteca e mantém uma lista de livros. Ela possui métodos para adicionar livros à biblioteca, listar todos os livros, emprestar um livro e devolver um livro.

No exemplo de uso, criamos uma nova biblioteca, adicionamos alguns livros, listamos os livros, emprestamos e devolvemos um livro e, em seguida, listamos novamente os livros para verificar as alterações.

Espero que este exemplo atenda às suas expectativas!