Claro! Aqui está um código complexo em Erlang que implementa um algoritmo de recomendação de filmes com base nas preferências do usuário:

```erlang
-module(recomendacao_filmes).
-export([recomendar_filmes/2]).

%% Função principal que recebe as preferências do usuário e retorna uma lista de filmes recomendados
recomendar_filmes(PreferenciasUsuario, Filmes) ->
    FilmesRelevantes = filtrar_filmes_relevantes(PreferenciasUsuario, Filmes),
    FilmesOrdenados = ordenar_filmes(FilmesRelevantes),
    FilmesRecomendados = obter_top_n_filmes(FilmesOrdenados, 5),
    FilmesRecomendados.

%% Função que filtra os filmes relevantes com base nas preferências do usuário
filtrar_filmes_relevantes(PreferenciasUsuario, Filmes) ->
    lists:filter(fun(Filme) -> filme_relevante(PreferenciasUsuario, Filme) end, Filmes).

%% Função que verifica se um filme é relevante para o usuário com base em suas preferências
filme_relevante(PreferenciasUsuario, Filme) ->
    lists:member(genero(Filme), PreferenciasUsuario) orelse
    lists:member(ator_principal(Filme), PreferenciasUsuario) orelse
    lists:member(ano_lancamento(Filme), PreferenciasUsuario).

%% Função que ordena os filmes de acordo com sua relevância para o usuário
ordenar_filmes(Filmes) ->
    lists:sort(fun(Filme1, Filme2) -> relevancia(Filme1) > relevancia(Filme2) end, Filmes).

%% Função que calcula a relevância de um filme com base em suas características
relevancia(Filme) ->
    Genero = genero(Filme),
    AtorPrincipal = ator_principal(Filme),
    AnoLancamento = ano_lancamento(Filme),
    RelevanciaGenero = if Genero == "Ação" -> 10;
                        Genero == "Comédia" -> 8;
                        Genero == "Drama" -> 9;
                        true -> 5 end,
    RelevanciaAtor = if AtorPrincipal == "Tom Hanks" -> 10;
                       AtorPrincipal == "Brad Pitt" -> 9;
                       AtorPrincipal == "Emma Stone" -> 8;
                       true -> 5 end,
    RelevanciaAno = if AnoLancamento > 2010 -> 8;
                     AnoLancamento > 2000 -> 7;
                     AnoLancamento > 1990 -> 6;
                     true -> 5 end,
    RelevanciaGenero + RelevanciaAtor + RelevanciaAno.

%% Função que retorna os N primeiros filmes da lista ordenada
obter_top_n_filmes(Filmes, N) ->
    lists:sublist(Filmes, 1, N).

%% Dados fictícios de exemplo
genero({titulo: "Filme 1", genero: "Ação", ator_principal: "Tom Hanks", ano_lancamento: 2015}).
genero({titulo: "Filme 2", genero: "Comédia", ator_principal: "Brad Pitt", ano_lancamento: 2008}).
genero({titulo: "Filme 3", genero: "Drama", ator_principal: "Emma Stone", ano_lancamento: 1995}).
genero({titulo: "Filme 4", genero: "Ação", ator_principal: "Tom Hanks", ano_lancamento: 2010}).
genero({titulo: "Filme 5", genero: "Comédia", ator_principal: "Emma Stone", ano_lancamento: 2012}).
genero({titulo: "Filme 6", genero: "Drama", ator_principal: "Brad Pitt", ano_lancamento: 1998}).

%% Exemplo de uso:
PreferenciasUsuario = ["Ação", "Brad Pitt", 2000],
Filmes = [{titulo: "Filme 1", genero: "Ação", ator_principal: "Tom Hanks", ano_lancamento: 2015},
          {titulo: "Filme 2", genero: "Comédia", ator_principal: "Brad Pitt", ano_lancamento: 2008},
          {titulo: "Filme 3", genero: "Drama", ator_principal: "Emma Stone", ano_lancamento: 1995},
          {titulo: "Filme 4", genero: "Ação", ator_principal: "Tom Hanks", ano_lancamento: 2010},
          {titulo: "Filme 5", genero: "Comédia", ator_principal: "Emma Stone", ano_lancamento: 2012},
          {titulo: "Filme 6", genero: "Drama", ator_principal: "Brad Pitt", ano_lancamento: 1998}],
recomendar_filmes(PreferenciasUsuario, Filmes).
```

Este código implementa um sistema simples de recomendação de filmes, baseado em preferências do usuário. Ele filtra os filmes relevantes, ordena-os com base em sua relevância e retorna os N melhores filmes recomendados. Claro, você pode personalizar as preferências do usuário e os dados dos filmes para testar diferentes cenários.