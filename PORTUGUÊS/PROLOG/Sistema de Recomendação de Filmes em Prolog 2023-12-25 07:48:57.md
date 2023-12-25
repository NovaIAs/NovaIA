Claro! Aqui está um código complexo em Prolog que implementa um sistema de recomendação de filmes com base nas preferências do usuário:

```
% Base de conhecimento com filmes e suas categorias
filme('O Poderoso Chefão', ['Drama', 'Crime']).
filme('Interestelar', ['Ficção Científica', 'Aventura']).
filme('A Origem', ['Ficção Científica', 'Ação']).
filme('Pulp Fiction', ['Drama', 'Crime']).
filme('Clube da Luta', ['Drama', 'Ação']).
filme('Matrix', ['Ficção Científica', 'Ação']).
filme('Os Infiltrados', ['Drama', 'Crime']).
filme('De Volta para o Futuro', ['Ficção Científica', 'Aventura']).
filme('Cidade de Deus', ['Drama', 'Crime']).
filme('Star Wars: Episódio IV', ['Ficção Científica', 'Aventura']).

% Base de conhecimento com as preferências do usuário
preferencia(joao, ['Drama', 'Crime', 'Ficção Científica']).
preferencia(maria, ['Aventura', 'Drama']).
preferencia(pedro, ['Ação', 'Ficção Científica']).
preferencia(ana, ['Comédia', 'Romance']).

% Predicado para calcular a similaridade entre as preferências do usuário e um filme
similaridade(Usuario, Filme, Similaridade) :-
    preferencia(Usuario, Preferencias),
    filme(Filme, Categorias),
    countSimilar(Preferencias, Categorias, Similaridade).

% Predicado auxiliar para contar quantas categorias em comum existem entre as preferências do usuário e um filme
countSimilar([], _, 0).
countSimilar([P|Preferencias], Categorias, Similaridade) :-
    member(P, Categorias),
    countSimilar(Preferencias, Categorias, Resto),
    Similaridade is Resto + 1.
countSimilar([_|Preferencias], Categorias, Similaridade) :-
    countSimilar(Preferencias, Categorias, Similaridade).

% Predicado para recomendar filmes para um usuário com base em suas preferências
recomendar(Usuario, Recomendacao) :-
    preferencia(Usuario, _), % Verifica se o usuário existe na base de conhecimento
    findall(Filme-Similaridade, (filme(Filme, _), similaridade(Usuario, Filme, Similaridade)), Recomendacoes),
    sort(2, @>=, Recomendacoes, RecomendacoesOrdenadas),
    member(Recomendacao-_, RecomendacoesOrdenadas).

% Exemplo de uso:
?- recomendar(joao, Recomendacao).
```

Explicação do código:

1. A base de conhecimento contém informações sobre filmes e suas categorias.
2. A base de conhecimento também contém informações sobre as preferências de diferentes usuários.
3. O predicado `similaridade/3` calcula a similaridade entre as preferências de um usuário e um filme, contando quantas categorias em comum existem.
4. O predicado `recomendar/2` recebe o nome de um usuário e retorna uma recomendação de filme com base em suas preferências.
5. A recomendação é feita listando todos os filmes, calculando a similaridade para cada um deles e ordenando as recomendações de forma decrescente.
6. Por fim, a recomendação é retornado como resposta.

Espero que este código atenda às suas expectativas!