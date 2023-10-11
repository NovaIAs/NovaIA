Claro! Aqui está um exemplo de um código complexo em PROLOG que envolve a implementação de um sistema de recomendação de filmes:

```prolog
% Base de dados de filmes e seus gêneros
filme('O Poderoso Chefão', ['Crime', 'Drama']).
filme('Interestelar', ['Ficção Científica', 'Aventura']).
filme('A Origem', ['Ficção Científica', 'Ação', 'Suspense']).
filme('Clube da Luta', ['Drama', 'Ação']).
filme('O Senhor dos Anéis', ['Fantasia', 'Aventura']).
filme('Pulp Fiction', ['Crime', 'Drama']).
filme('Matrix',['Ficção Científica','Ação']).
filme('Cidade de Deus', ['Crime', 'Drama']).
filme('O Iluminado', ['Suspense', 'Terror']).
filme('De Volta para o Futuro', ['Ficção Científica', 'Aventura']).

% Predicado para recomendar filmes com base em um gênero específico
recomenda_filme(Genre, RecommendedMovies) :-
    findall(Movie, (filme(Movie, Genres), member(Genre, Genres)), RecommendedMovies).

% Predicado para recomendar filmes com base em uma lista de gêneros
recomenda_filmes_por_generos([], []).
recomenda_filmes_por_generos([Genre|Rest], RecommendedMovies) :-
    recomenda_filme(Genre, Movies),
    recomenda_filmes_por_generos(Rest, RemainingMovies),
    append(Movies, RemainingMovies, RecommendedMovies).

% Exemplo de uso
?- recomenda_filmes_por_generos(['Crime', 'Drama', 'Suspense'], RecommendedMovies).
% A consulta acima retornará uma lista de filmes que são dos gêneros 'Crime', 'Drama' e 'Suspense'.
```

Neste código, começamos definindo uma base de dados de filmes e seus respectivos gêneros. Em seguida, temos o predicado `recomenda_filme/2`, que recebe um gênero como entrada e retorna uma lista de filmes que possuem esse gênero. 

Em seguida, temos o predicado `recomenda_filmes_por_generos/2`, que recebe uma lista de gêneros como entrada e retorna uma lista de filmes que possuem pelo menos um desses gêneros. O predicado utiliza recursão para percorrer a lista de gêneros e chama o predicado `recomenda_filme/2` para cada gênero, concatenando as listas de filmes retornadas.

Por fim, temos um exemplo de uso onde chamamos o predicado `recomenda_filmes_por_generos/2` com uma lista de gêneros específicos e obtemos a lista de filmes recomendados.

Espero que este código complexo em PROLOG atenda às suas expectativas!