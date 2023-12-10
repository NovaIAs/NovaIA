Claro! Aqui está um código em Prolog que implementa um sistema de recomendação de filmes baseado em preferências do usuário. O código está dividido em diferentes módulos para facilitar a compreensão.

```prolog
% MÓDULO DE DADOS

% Definição dos filmes disponíveis
filme('O Senhor dos Anéis: A Sociedade do Anel', fantasia, 2001, 8.8).
filme('Harry Potter e a Pedra Filosofal', fantasia, 2001, 7.6).
filme('Vingadores: Guerra Infinita', acao, 2018, 8.5).
filme('Interestelar', ficcao_cientifica, 2014, 8.6).
filme('Pulp Fiction', drama, 1994, 8.9).
filme('Matrix', acao, 1999, 8.7).
filme('O Silêncio dos Inocentes', suspense, 1991, 8.6).
filme('A Origem', suspense, 2010, 8.8).
% Adicione mais filmes conforme necessário

% MÓDULO DE REGRAS

% Predicado que retorna todos os filmes de um determinado gênero
filmes_do_genero(Genero, Filmes) :-
    findall(Filme, filme(Filme, Genero, _, _), Filmes).

% Predicado que retorna todos os filmes lançados em um determinado ano
filmes_do_ano(Ano, Filmes) :-
    findall(Filme, filme(Filme, _, Ano, _), Filmes).

% Predicado que retorna todos os filmes com avaliação igual ou maior que uma determinada nota
filmes_com_avaliacao(MinNota, Filmes) :-
    findall(Filme, filme(Filme, _, _, Avaliacao), Avaliacao >= MinNota, Filmes).

% Predicado que retorna a média das avaliações de todos os filmes
media_avaliacao(Media) :-
    findall(Avaliacao, filme(_, _, _, Avaliacao), Avaliacoes),
    length(Avaliacoes, Total),
    sum_list(Avaliacoes, Soma),
    Media is Soma / Total.

% MÓDULO DE INTERAÇÃO COM O USUÁRIO

% Predicado para solicitar ao usuário que escolha um gênero de filme
solicitar_genero(Gen) :-
    write('Por favor, escolha um gênero de filme: '), nl,
    write('1. Fantasia'), nl,
    write('2. Ação'), nl,
    write('3. Ficção Científica'), nl,
    write('4. Drama'), nl,
    write('5. Suspense'), nl,
    read(Opcao),
    escolher_genero(Opcao, Gen).

% Predicado para mapear a escolha do usuário para o gênero correspondente
escolher_genero(1, fantasia).
escolher_genero(2, acao).
escolher_genero(3, ficcao_cientifica).
escolher_genero(4, drama).
escolher_genero(5, suspense).

% Predicado para solicitar ao usuário que escolha um ano de lançamento
solicitar_ano(Ano) :-
    write('Por favor, escolha um ano de lançamento (1980-2022): '), nl,
    read(Ano), validacao_ano(Ano).

% Predicado para validar a escolha do usuário para o ano de lançamento
validacao_ano(Ano) :-
    Ano >= 1980,
    Ano =< 2022.

% Predicado para solicitar ao usuário que escolha uma avaliação mínima
solicitar_avaliacao(MinNota) :-
    write('Por favor, escolha uma avaliação mínima (0-10): '), nl,
    read(MinNota), validacao_avaliacao(MinNota).

% Predicado para validar a escolha do usuário para a avaliação mínima
validacao_avaliacao(MinNota) :-
    MinNota >= 0,
    MinNota =< 10.

% Predicado para recomendar filmes com base nas preferências do usuário
recomendar_filmes :-
    write('Bem-vindo ao sistema de recomendação de filmes!'), nl,
    solicitar_genero(Genero),
    solicitar_ano(Ano),
    solicitar_avaliacao(MinNota),
    filmes_do_genero(Genero, FilmesGenero),
    filmes_do_ano(Ano, FilmesAno),
    filmes_com_avaliacao(MinNota, FilmesAvaliacao),
    intersection(FilmesGenero, FilmesAno, Filmes1),
    intersection(Filmes1, FilmesAvaliacao, FilmesRecomendados),
    write('Aqui estão alguns filmes que você pode gostar:'), nl,
    imprimir_filmes(FilmesRecomendados).

% Predicado para imprimir a lista de filmes recomendados
imprimir_filmes([]).
imprimir_filmes([Filme | Filmes]) :-
    write(Filme), nl,
    imprimir_filmes(Filmes).
```

Explicação do código:
- O código está dividido em três módulos principais: o módulo de dados, que contém os filmes disponíveis e suas informações; o módulo de regras, que define os predicados para filtrar e calcular informações sobre os filmes; e o módulo de interação com o usuário, que solicita as preferências do usuário e recomenda filmes com base nelas.
- O módulo de dados contém fatos que descrevem os filmes disponíveis. Cada fato tem o formato `filme(Título, Gênero, Ano, Avaliação)`.
- O módulo de regras contém predicados que filtram os filmes com base em critérios como gênero, ano e avaliação. Os predicados `filmes_do_genero`, `filmes_do_ano` e `filmes_com_avaliacao` retornam listas de filmes que correspondem aos critérios especificados.
- O predicado `media_avaliacao` calcula a média das avaliações de todos os filmes disponíveis.
- O módulo de interação com o usuário contém os predicados que solicitam as preferências do usuário e interagem com ele. Os predicados `solicitar_genero`, `solicitar_ano` e `solicitar_avaliacao` solicitam ao usuário que escolha um gênero, um ano e uma avaliação mínima, respectivamente.
- O predicado `recomendar_filmes` é o ponto de entrada do sistema de recomendação. Ele chama os predicados de interação com o usuário e os predicados de regra para filtrar a lista de filmes recomendados com base nas preferências do usuário. Em seguida, imprime a lista de filmes recomendados.

Espero que este código em Prolog atenda às suas expectativas!