Claro! Aqui está um exemplo de código em Smalltalk que implementa um sistema de gerenciamento de biblioteca:

```
"Definindo as classes principais"

Class subclass: #Biblioteca
    instanceVariableNames: 'livros usuarios emprestimos'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Biblioteca'

Class subclass: #Livro
    instanceVariableNames: 'titulo autor status'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Biblioteca'

Class subclass: #Usuario
    instanceVariableNames: 'nome emprestimos'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Biblioteca'

Class subclass: #Emprestimo
    instanceVariableNames: 'livro usuario dataEmprestimo dataDevolucao'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Biblioteca'

"Definindo métodos para as classes"

"Classe Biblioteca"
Biblioteca >> initialize
    super initialize.
    livros := OrderedCollection new.
    usuarios := OrderedCollection new.
    emprestimos := OrderedCollection new.

Biblioteca >> adicionarLivro: umLivro
    livros add: umLivro.

Biblioteca >> removerLivro: umLivro
    livros remove: umLivro.

Biblioteca >> adicionarUsuario: umUsuario
    usuarios add: umUsuario.

Biblioteca >> removerUsuario: umUsuario
    usuarios remove: umUsuario.

Biblioteca >> emprestarLivro: umLivro paraUsuario: umUsuario
    | emprestimo |
    emprestimo := Emprestimo new
        livro: umLivro;
        usuario: umUsuario;
        dataEmprestimo: Date today.
    emprestimos add: emprestimo.
    umLivro status: 'Emprestado'.

Biblioteca >> devolverLivro: umLivro
    | emprestimo |
    emprestimo := emprestimos detect: [:e | e livro = umLivro].
    emprestimo dataDevolucao: Date today.
    umLivro status: 'Disponível'.

Biblioteca >> listarLivrosDisponiveis
    livros select: [:livro | livro status = 'Disponível'].

Biblioteca >> listarLivrosEmprestados
    livros select: [:livro | livro status = 'Emprestado'].

Biblioteca >> listarLivrosAtrasados
    | hoje |
    hoje := Date today.
    emprestimos select: [:emprestimo | hoje > emprestimo dataDevolucao].

"Classe Livro"
Livro >> initialize
    super initialize.
    status := 'Disponível'.

Livro >> status: umStatus
    status := umStatus.

"Classe Usuario"
Usuario >> initialize
    super initialize.
    emprestimos := OrderedCollection new.

Usuario >> emprestimos: umaColecao
    emprestimos := umaColecao.

Usuario >> adicionarEmprestimo: umEmprestimo
    emprestimos add: umEmprestimo.

Usuario >> removerEmprestimo: umEmprestimo
    emprestimos remove: umEmprestimo.

"Classe Emprestimo"
Emprestimo >> initialize
    super initialize.
    dataEmprestimo := Date today.

Emprestimo >> livro: umLivro
    livro := umLivro.

Emprestimo >> usuario: umUsuario
    usuario := umUsuario.

Emprestimo >> dataEmprestimo: umaData
    dataEmprestimo := umaData.

Emprestimo >> dataDevolucao: umaData
    dataDevolucao := umaData.

"Exemplo de uso"
| biblioteca livro1 livro2 usuario1 |
biblioteca := Biblioteca new.

livro1 := Livro new
    titulo: 'O Pequeno Príncipe';
    autor: 'Antoine de Saint-Exupéry'.

livro2 := Livro new
    titulo: 'Dom Quixote';
    autor: 'Miguel de Cervantes'.

usuario1 := Usuario new
    nome: 'João da Silva'.

biblioteca adicionarLivro: livro1.
biblioteca adicionarLivro: livro2.
biblioteca adicionarUsuario: usuario1.

biblioteca emprestarLivro: livro1 paraUsuario: usuario1.
biblioteca listarLivrosEmprestados. "Retorna uma coleção com o livro1"

biblioteca devolverLivro: livro1.
biblioteca listarLivrosDisponiveis. "Retorna uma coleção com todos os livros"

biblioteca listarLivrosAtrasados. "Retorna uma coleção vazia"
```

Neste código, temos as classes `Biblioteca`, `Livro`, `Usuario` e `Emprestimo`. A classe `Biblioteca` é responsável por gerenciar os livros, usuários e empréstimos. Os métodos `adicionarLivro`, `removerLivro`, `adicionarUsuario`, `removerUsuario`, `emprestarLivro`, `devolverLivro`, `listarLivrosDisponiveis`, `listarLivrosEmprestados` e `listarLivrosAtrasados` permitem realizar as operações necessárias em uma biblioteca.

Cada classe possui seus próprios métodos para realizar as ações específicas. Por exemplo, a classe `Livro` possui o método `status:` que permite definir o status do livro como "Disponível" ou "Emprestado". A classe `Usuario` possui o método `adicionarEmprestimo` para adicionar um empréstimo à coleção de empréstimos do usuário.

No exemplo de uso no final, criamos uma biblioteca, dois livros e um usuário. Em seguida, adicionamos os livros à biblioteca e emprestamos o livro1 para o usuário1. Após isso, listamos os livros emprestados, devolvemos o livro1 e listamos os livros disponíveis.

Espero que este exemplo tenha atendido às suas expectativas!