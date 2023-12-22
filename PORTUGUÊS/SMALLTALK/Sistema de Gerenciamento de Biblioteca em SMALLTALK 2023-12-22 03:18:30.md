Claro! Aqui está um exemplo de código em SMALLTALK que implementa um sistema de gerenciamento de biblioteca:

```
| biblioteca livros usuarios |

biblioteca := Dictionary new.
livros := OrderedCollection new.
usuarios := OrderedCollection new.

"Definindo a classe Livro"
Object subclass: #Livro
    instanceVariableNames: 'titulo autor disponivel'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Biblioteca'.

"Definindo a classe Usuario"
Object subclass: #Usuario
    instanceVariableNames: 'nome emprestimos'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Biblioteca'.

"Implementando os métodos da classe Livro"
Livro>>initialize
    super initialize.
    disponivel := true.

Livro>>emprestar
    disponivel := false.

Livro>>devolver
    disponivel := true.

"Implementando os métodos da classe Usuario"
Usuario>>initialize
    super initialize.
    emprestimos := OrderedCollection new.

Usuario>>emprestarLivro: umLivro
    emprestimos add: umLivro.

Usuario>>devolverLivro: umLivro
    emprestimos remove: umLivro.

"Implementando os métodos do sistema"
sistemaBiblioteca
    | opcao |
    [opcao notNil] whileTrue: [
        Transcript
            cr; show: '----- BIBLIOTECA -----';
            cr; show: '1. Cadastrar livro';
            cr; show: '2. Cadastrar usuário';
            cr; show: '3. Emprestar livro';
            cr; show: '4. Devolver livro';
            cr; show: '5. Sair';
            cr; show: 'Digite a opção desejada: '.
        opcao := FileStream stdin nextLine asNumber.
        
        opcao = 1 ifTrue: [
            | titulo autor livro |
            Transcript cr; show: '----- CADASTRAR LIVRO -----';
            Transcript cr; show: 'Digite o título do livro: '.
            titulo := FileStream stdin nextLine.
            Transcript cr; show: 'Digite o autor do livro: '.
            autor := FileStream stdin nextLine.
            livro := Livro new.
            livro titulo: titulo; autor: autor.
            livros add: livro.
            biblioteca at: livro put: livro.
        ].
        
        opcao = 2 ifTrue: [
            | nome usuario |
            Transcript cr; show: '----- CADASTRAR USUÁRIO -----';
            Transcript cr; show: 'Digite o nome do usuário: '.
            nome := FileStream stdin nextLine.
            usuario := Usuario new.
            usuario nome: nome.
            usuarios add: usuario.
        ].
        
        opcao = 3 ifTrue: [
            | livro usuario |
            Transcript cr; show: '----- EMPRESTAR LIVRO -----';
            Transcript cr; show: 'Digite o título do livro: '.
            (livros detect: [:each | each titulo = (FileStream stdin nextLine)]) ifNotNil: [:livroEncontrado |
                livro := livroEncontrado.
                livro disponivel ifTrue: [
                    Transcript cr; show: 'Digite o nome do usuário: '.
                    (usuarios detect: [:each | each nome = (FileStream stdin nextLine)]) ifNotNil: [:usuarioEncontrado |
                        usuario := usuarioEncontrado.
                        usuario emprestarLivro: livro.
                        livro emprestar.
                        Transcript cr; show: 'Livro emprestado com sucesso!'.
                    ] ifNil: [
                        Transcript cr; show: 'Usuário não encontrado!'.
                    ].
                ] ifFalse: [
                    Transcript cr; show: 'Livro indisponível!'.
                ].
            ] ifNil: [
                Transcript cr; show: 'Livro não encontrado!'.
            ].
        ].
        
        opcao = 4 ifTrue: [
            | livro usuario |
            Transcript cr; show: '----- DEVOLVER LIVRO -----';
            Transcript cr; show: 'Digite o título do livro: '.
            (livros detect: [:each | each titulo = (FileStream stdin nextLine)]) ifNotNil: [:livroEncontrado |
                livro := livroEncontrado.
                livro disponivel ifFalse: [
                    Transcript cr; show: 'Digite o nome do usuário: '.
                    (usuarios detect: [:each | each nome = (FileStream stdin nextLine)]) ifNotNil: [:usuarioEncontrado |
                        usuario := usuarioEncontrado.
                        usuario devolverLivro: livro.
                        livro devolver.
                        Transcript cr; show: 'Livro devolvido com sucesso!'.
                    ] ifNil: [
                        Transcript cr; show: 'Usuário não encontrado!'.
                    ].
                ] ifTrue: [
                    Transcript cr; show: 'O livro já está disponível!'.
                ].
            ] ifNil: [
                Transcript cr; show: 'Livro não encontrado!'.
            ].
        ].
        
        opcao = 5 ifTrue: [
            Transcript cr; show: 'Saindo do sistema...';
        ].
    ].
```

Explicação do código:

Este código em SMALLTALK implementa um sistema de gerenciamento de biblioteca. Ele cria duas classes, `Livro` e `Usuario`, que são subclasses da classe `Object`. Cada livro tem um título, um autor e uma variável booleana chamada `disponivel`, que indica se o livro está disponível para empréstimo ou não. Cada usuário tem um nome e uma coleção de livros emprestados.

A classe `Livro` possui três métodos: `initialize`, `emprestar` e `devolver`. O método `initialize` é usado para inicializar as variáveis do livro, e o método `emprestar` e `devolver` são usados para marcar o livro como emprestado ou disponível, respectivamente.

A classe `Usuario` também possui três métodos: `initialize`, `emprestarLivro:` e `devolverLivro:`. O método `initialize` é usado para inicializar as variáveis do usuário, o método `emprestarLivro:` é usado quando o usuário empresta um livro, e o método `devolverLivro:` é usado quando o usuário devolve um livro.

O método `sistemaBiblioteca` é o ponto de entrada do sistema. Ele exibe um menu para o usuário com várias opções, como cadastrar livro, cadastrar usuário, emprestar livro, devolver livro e sair do sistema. Dependendo da opção selecionada, o código executa a funcionalidade correspondente. Por exemplo, se o usuário selecionar a opção "Cadastrar livro", o código pede ao usuário para digitar o título e o autor do livro, cria um novo objeto `Livro` com essas informações e adiciona o livro à coleção `livros` e ao dicionário `biblioteca`.

O sistema continua executando até que o usuário selecione a opção "Sair".