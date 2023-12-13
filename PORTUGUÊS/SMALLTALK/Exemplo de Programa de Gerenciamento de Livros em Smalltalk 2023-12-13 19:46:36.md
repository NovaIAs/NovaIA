Claro! Aqui está um exemplo de código em Smalltalk que implementa um programa de gerenciamento de livros em uma biblioteca:

```smalltalk
"Definindo a classe Livro"
Object subclass: #Livro
    instanceVariableNames: 'titulo autor anoPublicacao'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Biblioteca'

"Definindo os métodos da classe Livro"
Livro>>initialize
    super initialize.
    titulo := ''.
    autor := ''.
    anoPublicacao := 0.

Livro>>setTitulo: aString
    titulo := aString.

Livro>>setAutor: aString
    autor := aString.

Livro>>setAnoPublicacao: aNumber
    anoPublicacao := aNumber.

Livro>>getTitulo
    ^titulo.

Livro>>getAutor
    ^autor.

Livro>>getAnoPublicacao
    ^anoPublicacao.

"Definindo a classe Biblioteca"
Object subclass: #Biblioteca
    instanceVariableNames: 'livros'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Biblioteca'

"Definindo os métodos da classe Biblioteca"
Biblioteca>>initialize
    super initialize.
    livros := OrderedCollection new.

Biblioteca>>adicionarLivro: umLivro
    livros add: umLivro.

Biblioteca>>removerLivro: umLivro
    livros remove: umLivro ifAbsent: [].

Biblioteca>>listarLivros
    livros do: [:livro | 
        Transcript show: 'Título: ', livro getTitulo;
                      show: 'Autor: ', livro getAutor;
                      show: 'Ano de Publicação: ', livro getAnoPublicacao asString;
                      cr].

"Exemplo de uso do programa"
| biblioteca livro1 livro2 |
biblioteca := Biblioteca new.

livro1 := Livro new.
livro1 setTitulo: 'O Senhor dos Anéis'.
livro1 setAutor: 'J.R.R. Tolkien'.
livro1 setAnoPublicacao: 1954.

livro2 := Livro new.
livro2 setTitulo: 'Harry Potter e a Pedra Filosofal'.
livro2 setAutor: 'J.K. Rowling'.
livro2 setAnoPublicacao: 1997.

biblioteca adicionarLivro: livro1.
biblioteca adicionarLivro: livro2.

biblioteca listarLivros.
```

Este código em Smalltalk implementa duas classes: "Livro" e "Biblioteca". A classe "Livro" possui variáveis de instância para armazenar o título, autor e ano de publicação de um livro, e métodos para configurar e obter essas informações.

A classe "Biblioteca" possui uma coleção ordenada de livros e métodos para adicionar, remover e listar os livros presentes na biblioteca. O método "listarLivros" utiliza o objeto Transcript para exibir os detalhes de cada livro na janela de transcrição.

No exemplo de uso do programa, criamos uma instância da classe Biblioteca, dois livros e os adicionamos à biblioteca. Em seguida, chamamos o método "listarLivros" para exibir os detalhes dos livros na janela de transcrição.

Espero que este exemplo tenha sido útil!