Claro! Aqui está um exemplo de um código complexo em Prolog que cria um sistema de recomendação de filmes com base nas preferências do usuário:

```prolog
% Base de conhecimento contendo os filmes disponíveis e suas categorias
filme('Matrix', ['Ação', 'Ficção Científica']).
filme('Interestelar', ['Aventura', 'Ficção Científica']).
filme('O Poderoso Chefão', ['Drama', 'Crime']).
filme('Os Vingadores', ['Ação', 'Aventura']).
filme('Clube da Luta', ['Drama', 'Ação']).
filme('Cidade de Deus', ['Drama', 'Crime']).
filme('A Origem', ['Ação', 'Ficção Científica']).
filme('Pulp Fiction', ['Drama', 'Crime']).
filme('O Senhor dos Anéis', ['Aventura', 'Fantasia']).

% Base de conhecimento contendo as preferências dos usuários
preferencia('João', ['Ação', 'Ficção Científica']).
preferencia('Maria', ['Drama', 'Crime']).
preferencia('Pedro', ['Aventura', 'Fantasia']).

% Predicado para recomendar filmes com base nas preferências do usuário
recomendar_filmes(Usuario, FilmesRecomendados) :-
    preferencia(Usuario, Preferencias),
    findall(Filme, (filme(Filme, Categorias), intersection(Categorias, Preferencias, Filmes)), FilmesRecomendados).

% Exemplo de consulta para recomendar filmes para o usuário 'João'
?- recomendar_filmes('João', FilmesRecomendados).
```

Explicação do código:
1. A base de conhecimento contém informações sobre os filmes disponíveis e suas categorias, bem como as preferências dos usuários.
2. O predicado `recomendar_filmes/2` recebe como entrada o nome do usuário e retorna uma lista de filmes recomendados com base nas preferências desse usuário.
3. O predicado `preferencia/2` relaciona o nome do usuário com suas preferências de filme.
4. O predicado `filme/2` relaciona o nome do filme com suas categorias.
5. O predicado `findall/3` é usado para encontrar todos os filmes que possuem pelo menos uma categoria em comum com as preferências do usuário.
6. Na consulta de exemplo, estamos recomendando filmes para o usuário 'João', que tem preferências por filmes de 'Ação' e 'Ficção Científica'.

Esse é apenas um exemplo de código complexo em Prolog que mostra como implementar um sistema de recomendação de filmes simples. Você pode expandir esse código adicionando mais funcionalidades, como avaliações dos filmes pelos usuários ou considerando outras características dos filmes.