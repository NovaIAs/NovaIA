Claro! Aqui está um código complexo em Prolog que implementa um motor de busca de livros em uma biblioteca virtual. O programa utiliza uma base de conhecimento de livros e permite que o usuário pesquise por título, autor ou categoria.

```prolog
% Base de conhecimento dos livros
livro('O Pequeno Príncipe', 'Antoine de Saint-Exupéry', 'Fantasia').
livro('Dom Quixote', 'Miguel de Cervantes', 'Aventura').
livro('1984', 'George Orwell', 'Distopia').
livro('O Senhor dos Anéis', 'J.R.R. Tolkien', 'Fantasia').
livro('O Alquimista', 'Paulo Coelho', 'Autoajuda').
livro('Cem Anos de Solidão', 'Gabriel García Márquez', 'Realismo Mágico').

% Predicado para pesquisar um livro pelo título
pesquisar_por_titulo(Titulo, Autor, Categoria) :-
    livro(Titulo, Autor, Categoria).

% Predicado para pesquisar um livro pelo autor
pesquisar_por_autor(Autor, Titulo, Categoria) :-
    livro(Titulo, Autor, Categoria).

% Predicado para pesquisar um livro pela categoria
pesquisar_por_categoria(Categoria, Titulo, Autor) :-
    livro(Titulo, Autor, Categoria).

% Predicado para exibir o resultado da pesquisa
exibir_resultado(Titulo, Autor, Categoria) :-
    writeln('Livro encontrado:'),
    writeln('Título: '),
    writeln(Titulo),
    writeln('Autor: '),
    writeln(Autor),
    writeln('Categoria: '),
    writeln(Categoria).

% Predicado principal para interagir com o usuário
inicio :-
    writeln('Bem-vindo à biblioteca virtual!'),
    repeat,
    writeln('O que você deseja fazer?'),
    writeln('1 - Pesquisar livro por título'),
    writeln('2 - Pesquisar livro por autor'),
    writeln('3 - Pesquisar livro por categoria'),
    writeln('0 - Sair'),
    read(Opcao),
    (Opcao = 1 -> pesquisar_por_titulo; 
     Opcao = 2 -> pesquisar_por_autor; 
     Opcao = 3 -> pesquisar_por_categoria; 
     Opcao = 0 -> writeln('Obrigado por utilizar a biblioteca virtual!'), !),
    writeln('Digite o termo de pesquisa: '),
    read(Termo),
    call(Opcao, Termo, Titulo, Autor, Categoria),
    exibir_resultado(Titulo, Autor, Categoria),
    nl,
    fail.
```

Explicação do código:
- A primeira parte do código é a base de conhecimento dos livros, onde cada livro é representado por um fato com o título, autor e categoria.
- Em seguida, temos três predicados (`pesquisar_por_titulo/3`, `pesquisar_por_autor/3` e `pesquisar_por_categoria/3`) que recebem um termo de pesquisa e retornam o título, autor e categoria do livro correspondente.
- O predicado `exibir_resultado/3` é responsável por exibir o resultado da pesquisa na tela.
- O predicado `inicio/0` é o ponto de entrada do programa. Ele exibe um menu para o usuário escolher a opção desejada e realiza a pesquisa de acordo com a opção selecionada. O programa repete até que o usuário escolha a opção de sair (0).
- O programa utiliza o predicado `repeat/0` e `fail/0` para criar um loop infinito que só é interrompido quando o usuário escolhe a opção de sair.

Esse código permite que o usuário pesquise por livros na biblioteca virtual de forma interativa, utilizando o Prolog como linguagem de programação.