Claro! Aqui está um código em Smalltalk, uma linguagem de programação orientada a objetos:

```smalltalk
"Este é um exemplo de código em Smalltalk que implementa um sistema de gerenciamento de biblioteca."

"Primeiro, criaremos uma classe chamada Livro para representar um livro na biblioteca."
Object subclass: #Livro
    instanceVariableNames: 'titulo autor anoPublicacao'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Biblioteca'

"Em seguida, vamos adicionar métodos para inicializar um livro, obter e definir o título, o autor e o ano de publicação."
Livro>>initialize: aTitulo autor: aAutor anoPublicacao: aAnoPublicacao
    titulo := aTitulo.
    autor := aAutor.
    anoPublicacao := aAnoPublicacao.

Livro>>titulo
    ^titulo

Livro>>autor
    ^autor

Livro>>anoPublicacao
    ^anoPublicacao

Livro>>setTitulo: aTitulo
    titulo := aTitulo

Livro>>setAutor: aAutor
    autor := aAutor

Livro>>setAnoPublicacao: aAnoPublicacao
    anoPublicacao := aAnoPublicacao

"Agora, vamos criar uma classe chamada Biblioteca para representar a biblioteca em si."
Object subclass: #Biblioteca
    instanceVariableNames: 'livros'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Biblioteca'

"Vamos adicionar métodos para inicializar a biblioteca e adicionar e remover livros."
Biblioteca>>initialize
    livros := OrderedCollection new.

Biblioteca>>adicionarLivro: umLivro
    livros add: umLivro

Biblioteca>>removerLivro: umLivro
    livros remove: umLivro

"Por fim, vamos adicionar um método para listar todos os livros da biblioteca."
Biblioteca>>listarLivros
    livros do: [:livro |
        Transcript show: 'Título: ', livro titulo, '; Autor: ', livro autor, '; Ano de Publicação: ', livro anoPublicacao printString; cr].
```

Neste código, criamos duas classes: `Livro` e `Biblioteca`. A classe `Livro` representa um livro na biblioteca e possui métodos para inicializar um livro, obter e definir o título, o autor e o ano de publicação. A classe `Biblioteca` representa a biblioteca em si e possui métodos para inicializar a biblioteca, adicionar e remover livros, e listar todos os livros da biblioteca.

Por exemplo, podemos usar esse código para criar uma biblioteca e adicionar alguns livros a ela:

```smalltalk
| biblioteca livro1 livro2 |
biblioteca := Biblioteca new.

livro1 := Livro new.
livro1 initialize: 'Dom Casmurro' autor: 'Machado de Assis' anoPublicacao: 1899.
biblioteca adicionarLivro: livro1.

livro2 := Livro new.
livro2 initialize: 'O Pequeno Príncipe' autor: 'Antoine de Saint-Exupéry' anoPublicacao: 1943.
biblioteca adicionarLivro: livro2.

biblioteca listarLivros.
```

Isso exibiria a seguinte saída no Transcript:

```
Título: Dom Casmurro; Autor: Machado de Assis; Ano de Publicação: 1899
Título: O Pequeno Príncipe; Autor: Antoine de Saint-Exupéry; Ano de Publicação: 1943
```

Espero que este exemplo tenha ajudado a entender como criar um sistema de gerenciamento de biblioteca em Smalltalk!