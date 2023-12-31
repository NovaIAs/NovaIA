Claro! Aqui está um exemplo de código em Smalltalk, que é uma linguagem orientada a objetos:

```smalltalk
"Este é um exemplo de código em Smalltalk que simula um sistema de biblioteca."

Object subclass: Biblioteca [
    | livros usuarios |

    Biblioteca class >> initialize [
        super initialize.
        livros := Dictionary new.
        usuarios := Dictionary new.
    ]

    Biblioteca >> cadastrarLivro: titulo autor [
        | livro |
        livro := Livro new.
        livro titulo: titulo.
        livro autor: autor.
        livros at: titulo put: livro.
    ]

    Biblioteca >> emprestarLivro: titulo paraUsuario: nome [
        | livro usuario |
        livro := livros at: titulo ifAbsent: [ ^'Livro não encontrado' ].
        usuario := usuarios at: nome ifAbsent: [
            usuario := Usuario new.
            usuario nome: nome.
            usuarios at: nome put: usuario.
        ].
        usuario emprestarLivro: livro.
    ]

    Biblioteca >> devolverLivro: titulo doUsuario: nome [
        | livro usuario |
        livro := livros at: titulo ifAbsent: [ ^'Livro não encontrado' ].
        usuario := usuarios at: nome ifAbsent: [ ^'Usuário não encontrado' ].
        usuario devolverLivro: livro.
    ]
]

Object subclass: Livro [
    | titulo autor disponivel |

    Livro >> titulo: aString [
        titulo := aString.
    ]

    Livro >> autor: aString [
        autor := aString.
    ]

    Livro >> disponivel [
        ^disponivel
    ]

    Livro >> emprestar [
        disponivel := false.
    ]

    Livro >> devolver [
        disponivel := true.
    ]
]

Object subclass: Usuario [
    | nome livrosEmprestados |

    Usuario >> nome: aString [
        nome := aString.
        livrosEmprestados := OrderedCollection new.
    ]

    Usuario >> emprestarLivro: livro [
        livrosEmprestados add: livro.
        livro emprestar.
    ]

    Usuario >> devolverLivro: livro [
        livrosEmprestados remove: livro ifAbsent: [ ^'Livro não encontrado' ].
        livro devolver.
    ]
]

"Exemplo de uso:"

Biblioteca initialize.
Biblioteca cadastrarLivro: 'O Pequeno Príncipe' autor: 'Antoine de Saint-Exupéry'.
Biblioteca cadastrarLivro: 'Dom Quixote' autor: 'Miguel de Cervantes'.

Biblioteca emprestarLivro: 'O Pequeno Príncipe' paraUsuario: 'João'.
Biblioteca emprestarLivro: 'Dom Quixote' paraUsuario: 'Maria'.

Biblioteca devolverLivro: 'O Pequeno Príncipe' doUsuario: 'João'.
```

Neste exemplo, criamos três classes: `Biblioteca`, `Livro` e `Usuario`. A classe `Biblioteca` possui métodos para cadastrar livros, emprestar livros e devolver livros. A classe `Livro` representa um livro, com atributos como título, autor e disponibilidade. A classe `Usuario` representa um usuário, com um nome e uma lista de livros emprestados.

No exemplo de uso, criamos uma instância da `Biblioteca`, cadastramos dois livros e emprestamos um deles para um usuário chamado João. Depois, devolvemos o livro emprestado por João.

Esse é apenas um exemplo simples de código em Smalltalk, mas a linguagem oferece muitas outras funcionalidades e recursos interessantes para programação orientada a objetos.