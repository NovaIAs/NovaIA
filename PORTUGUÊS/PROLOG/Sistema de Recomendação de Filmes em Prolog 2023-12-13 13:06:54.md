Claro! Vou criar um código em Prolog que implementa um sistema de recomendação de filmes baseado nos gostos do usuário. O sistema utiliza a técnica de filtragem colaborativa para sugerir filmes similares aos que o usuário já assistiu e gostou.

```prolog
% Base de conhecimento com os filmes e suas características
filme(laranja_mecanica, [crime, drama, ficcao]).
filme(pulp_fiction, [crime, drama]).
filme(matrix, [acao, ficcao]).
filme(vingadores, [acao, ficcao]).
filme(titanic, [romance, drama]).
filme(senhordosaneis, [aventura, fantasia]).
filme(harrypotter, [aventura, fantasia]).

% Base de conhecimento com as preferências dos usuários
preferencia(joao, [laranja_mecanica, vingadores, titanic]).
preferencia(maria, [pulp_fiction, matrix, harrypotter]).
preferencia(pedro, [matrix, senhordosaneis]).
preferencia(ana, [titanic, harrypotter]).

% Predicado que retorna a lista de filmes recomendados para um usuário
recomendar_filmes(Usuario, Recomendacao) :-
    preferencia(Usuario, FilmesAssistidos),
    encontrar_filmes_similares(FilmesAssistidos, FilmesRecomendados),
    remover_assistidos(FilmesRecomendados, FilmesAssistidos, Recomendacao).

% Predicado que encontra os filmes similares aos que o usuário já assistiu
encontrar_filmes_similares(FilmesAssistidos, FilmesRecomendados) :-
    findall(Filme, (preferencia(_, Filmes), member(Filme, Filmes), member(Filme, FilmesAssistidos)), FilmesRecomendados).

% Predicado que remove os filmes já assistidos da lista de recomendação
remover_assistidos([], _, []).
remover_assistidos([Filme|Filmes], FilmesAssistidos, Recomendacao) :-
    member(Filme, FilmesAssistidos),
    remover_assistidos(Filmes, FilmesAssistidos, Recomendacao).
remover_assistidos([Filme|Filmes], FilmesAssistidos, [Filme|Recomendacao]) :-
    not(member(Filme, FilmesAssistidos)),
    remover_assistidos(Filmes, FilmesAssistidos, Recomendacao).
```

Explicação do código:

1. Definimos a base de conhecimento com os filmes e suas características. Cada filme é representado por um átomo e suas características são armazenadas em uma lista.
2. Em seguida, definimos a base de conhecimento com as preferências dos usuários. Cada usuário é representado por um átomo e suas preferências são armazenadas em uma lista de filmes.
3. O predicado `recomendar_filmes/2` recebe como parâmetro um usuário e retorna a lista de filmes recomendados para esse usuário.
4. Dentro do predicado `recomendar_filmes/2`, utilizamos o predicado `encontrar_filmes_similares/2` para encontrar os filmes similares aos que o usuário já assistiu.
5. O predicado `encontrar_filmes_similares/2` utiliza o predicado `findall/3` para encontrar todos os filmes que são preferidos por outros usuários e que também estão na lista de filmes assistidos pelo usuário atual.
6. Por fim, o predicado `remover_assistidos/3` é utilizado para remover os filmes já assistidos da lista de recomendação, retornando a lista final de filmes recomendados.

Espero que esse código possa te ajudar a entender melhor como implementar um sistema de recomendação em Prolog. Lembre-se de adaptá-lo conforme necessário e adicionar mais filmes e preferências para obter resultados mais precisos.