Claro! Aqui está um código em Smalltalk bastante elaborado e explicado em português:

```
"Este é um exemplo de código em Smalltalk que implementa um sistema de gerenciamento de biblioteca virtual."

"Primeiro, definimos uma classe chamada 'Livro' para representar cada livro na biblioteca."
Object subclass: #Livro
    instanceVariableNames: 'titulo autor anoPublicacao'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Biblioteca'

"Em seguida, definimos os métodos da classe 'Livro' para acessar e modificar os atributos do livro."
Livro>>titulo
    ^ titulo

Livro>>autor
    ^ autor

Livro>>anoPublicacao
    ^ anoPublicacao

Livro>>titulo: novoTitulo
    titulo := novoTitulo

Livro>>autor: novoAutor
    autor := novoAutor

Livro>>anoPublicacao: novoAno
    anoPublicacao := novoAno

"Depois, criamos uma classe chamada 'Biblioteca' para representar a coleção de livros."
Object subclass: #Biblioteca
    instanceVariableNames: 'livros'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Biblioteca'

"Definimos um método de inicialização para a classe 'Biblioteca', que cria uma lista vazia de livros."
Biblioteca>>initialize
    livros := OrderedCollection new

"Agora, adicionamos métodos para adicionar e remover livros da biblioteca."
Biblioteca>>adicionarLivro: umLivro
    livros add: umLivro

Biblioteca>>removerLivro: umLivro
    livros remove: umLivro

"Em seguida, criamos um método para listar todos os livros da biblioteca."
Biblioteca>>listarLivros
    livros do: [ :livro |
        Transcript show: livro titulo; cr ]

"Por fim, definimos um método de exemplo que demonstra o uso da biblioteca."
Biblioteca>>exemplo
    | livro1 livro2 biblioteca |
    
    livro1 := Livro new.
    livro1 titulo: 'Dom Casmurro'.
    livro1 autor: 'Machado de Assis'.
    livro1 anoPublicacao: 1899.
    
    livro2 := Livro new.
    livro2 titulo: '1984'.
    livro2 autor: 'George Orwell'.
    livro2 anoPublicacao: 1949.
    
    biblioteca := Biblioteca new.
    biblioteca adicionarLivro: livro1.
    biblioteca adicionarLivro: livro2.
    
    biblioteca listarLivros

"Agora podemos criar uma instância da classe 'Biblioteca' e chamar o método 'exemplo' para ver o resultado."
| minhaBiblioteca |
minhaBiblioteca := Biblioteca new.
minhaBiblioteca exemplo.
```

Neste exemplo, criamos duas classes: `Livro` e `Biblioteca`. A classe `Livro` representa os livros individuais na biblioteca, enquanto a classe `Biblioteca` representa a coleção de livros.

No código, definimos os métodos necessários para acessar e modificar os atributos de um livro, como título, autor e ano de publicação. Também implementamos métodos na classe `Biblioteca` para adicionar, remover e listar os livros.

O método `exemplo` na classe `Biblioteca` demonstra o uso da biblioteca, criando dois livros, adicionando-os à biblioteca e, em seguida, listando todos os livros presentes.

Finalmente, criamos uma instância da classe `Biblioteca` e chamamos o método `exemplo` para ver o resultado no console.