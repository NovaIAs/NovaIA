Claro! Aqui está um exemplo de código complexo em Prolog que envolve uma implementação de um sistema de recomendação de filmes:

```
% Base de conhecimento com filmes e suas características
filme('Vingadores: Ultimato', ['Ação', 'Aventura', 'Fantasia'], 2019, 8.4).
filme('Interestelar', ['Aventura', 'Ficção Científica'], 2014, 8.6).
filme('O Poderoso Chefão', ['Crime', 'Drama'], 1972, 9.2).
filme('O Senhor dos Anéis: O Retorno do Rei', ['Ação', 'Aventura', 'Fantasia'], 2003, 8.9).
filme('Clube da Luta', ['Drama'], 1999, 8.8).
filme('O Labirinto do Fauno', ['Drama', 'Fantasia'], 2006, 8.2).
filme('Pulp Fiction: Tempo de Violência', ['Crime', 'Drama'], 1994, 8.9).
filme('Cidade de Deus', ['Crime', 'Drama'], 2002, 8.6).
filme('Matrix', ['Ação', 'Ficção Científica'], 1999, 8.7).
filme('O Silêncio dos Inocentes', ['Crime', 'Drama', 'Suspense'], 1991, 8.6).

% Função para recomendar filmes baseado em um gênero fornecido
recomendar_filme(Genero) :-
    findall(Nome, (filme(Nome, Generos, _, _), member(Genero, Generos)), Filmes),
    length(Filmes, NumFilmes),
    random(0, NumFilmes, Index),
    nth0(Index, Filmes, FilmeRecomendado),
    write('Recomendo assistir ao filme: '), write(FilmeRecomendado).

% Função para recomendar filmes baseado em um gênero e ano fornecidos
recomendar_filme(Genero, Ano) :-
    findall(Nome, (filme(Nome, Generos, AnoFilme, _), member(Genero, Generos), AnoFilme =< Ano), Filmes),
    length(Filmes, NumFilmes),
    random(0, NumFilmes, Index),
    nth0(Index, Filmes, FilmeRecomendado),
    write('Recomendo assistir ao filme: '), write(FilmeRecomendado).

% Exemplo de uso
:- initialization(main).

main :-
    write('Bem-vindo! Estou aqui para recomendar filmes. Escolha uma opção:\n'),
    write('1 - Recomendar filme por gênero\n'),
    write('2 - Recomendar filme por gênero e ano\n'),
    read(Opcao),
    (
        Opcao = 1 -> (
            write('Digite o gênero: '),
            read(Genero),
            recomendar_filme(Genero)
        );
        Opcao = 2 -> (
            write('Digite o gênero: '),
            read(Genero),
            write('Digite o ano: '),
            read(Ano),
            recomendar_filme(Genero, Ano)
        );
        write('Opção inválida')
    ),
    halt.
```

Este código em Prolog implementa um sistema simples de recomendação de filmes. A base de conhecimento contém informações sobre filmes, como título, gênero, ano de lançamento e classificação. Os filmes são armazenados como fatos usando o predicado `filme/4`.

Há duas regras `recomendar_filme/1` e `recomendar_filme/2` que recebem um gênero e um gênero e ano, respectivamente. Essas regras usam a função `findall/3` para encontrar todos os filmes que correspondem aos critérios de gênero e ano fornecidos. Em seguida, a função `length/2` é usada para obter o número total de filmes encontrados. A função `random/3` é usada para gerar um índice aleatório para escolher um filme da lista de filmes encontrados. A função `nth0/3` é usada para obter o filme recomendado com base no índice gerado aleatoriamente.

Dentro do `main/0`, o programa mostra um menu para o usuário escolher entre duas opções: recomendar filme por gênero ou recomendar filme por gênero e ano. Após a escolha da opção, o programa solicita ao usuário os dados necessários e chama a função apropriada para recomendar um filme.

Espero que este exemplo lhe ajude a entender como implementar sistemas de recomendação em Prolog!