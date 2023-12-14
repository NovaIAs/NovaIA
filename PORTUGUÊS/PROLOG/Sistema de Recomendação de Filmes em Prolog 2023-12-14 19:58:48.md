Claro! Abaixo está um código complexo em Prolog que implementa um sistema de recomendação de filmes com base nas preferências do usuário. O código está em português e inclui comentários explicativos para facilitar o entendimento:

```prolog
% Base de conhecimento contendo informações sobre os filmes
filme('O Poderoso Chefão', [crime, drama], 9.2).
filme('A Origem', [ação, ficção], 8.8).
filme('Os Suspeitos', [crime, mistério], 8.5).
filme('Interestelar', [aventura, ficção], 8.6).
filme('Clube da Luta', [drama], 8.8).
filme('Pulp Fiction', [crime, drama], 8.9).
filme('Cidade de Deus', [crime, drama], 8.7).
filme('O Senhor dos Anéis: A Sociedade do Anel', [aventura, fantasia], 8.8).
filme('Matrix', [ação, ficção], 8.7).
filme('O Silêncio dos Inocentes', [crime, suspense], 8.6).

% Regra que recomenda filmes com base nas preferências do usuário
recomendarFilmes(GenerosPreferidos, NotaMinima, FilmesRecomendados) :-
    % Encontra todos os filmes que correspondem aos gêneros preferidos
    findall(Filme, (filme(Filme, Generos, Nota), intersecao(GenerosPreferidos, Generos)), Filmes),
    % Filtra os filmes com nota maior ou igual à nota mínima
    filtrarPorNota(Filmes, NotaMinima, FilmesRecomendados).

% Regra que verifica a interseção entre duas listas
intersecao([], _, []).
intersecao([X|Resto], Lista, [X|Intersecao]) :- 
    member(X, Lista),
    intersecao(Resto, Lista, Intersecao).
intersecao([_|Resto], Lista, Intersecao) :- 
    intersecao(Resto, Lista, Intersecao).

% Regra que filtra os filmes por nota mínima
filtrarPorNota([], _, []).
filtrarPorNota([Filme|Resto], NotaMinima, [Filme|Filtrados]) :-
    filme(Filme, _, Nota),
    Nota >= NotaMinima,
    filtrarPorNota(Resto, NotaMinima, Filtrados).
filtrarPorNota([_|Resto], NotaMinima, Filtrados) :-
    filtrarPorNota(Resto, NotaMinima, Filtrados).
```

Neste código, temos uma base de conhecimento que contém informações sobre filmes, como título, gêneros e nota. A regra `recomendarFilmes/3` recebe como parâmetros os gêneros preferidos do usuário, a nota mínima desejada e retorna uma lista de filmes recomendados.

A regra `intersecao/3` é utilizada para encontrar a interseção entre a lista de gêneros preferidos do usuário e os gêneros de cada filme. A função `findall/3` é usada para encontrar todos os filmes que correspondem aos gêneros preferidos.

Em seguida, a regra `filtrarPorNota/3` é responsável por filtrar os filmes encontrados anteriormente com base na nota mínima desejada. A função `member/2` é usada para verificar se um filme está presente na lista de gêneros preferidos.

Por fim, é possível chamar a regra `recomendarFilmes/3` passando os parâmetros desejados para obter a lista de filmes recomendados.

Espero que este código atenda às suas exigências!