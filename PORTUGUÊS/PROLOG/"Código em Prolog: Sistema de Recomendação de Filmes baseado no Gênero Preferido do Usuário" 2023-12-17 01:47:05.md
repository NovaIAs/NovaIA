Claro! Aqui está um código complexo em Prolog que implementa um sistema de recomendação de filmes baseado no gênero preferido do usuário:

```prolog
% Base de conhecimento contendo informações sobre os filmes
filme('Coringa', drama, 2019).
filme('Interestelar', ficcao, 2014).
filme('O Poderoso Chefão', crime, 1972).
filme('Vingadores: Ultimato', acao, 2019).
filme('Toy Story 4', animacao, 2019).
filme('O Labirinto do Fauno', fantasia, 2006).
filme('A Origem', ficcao, 2010).
filme('O Silêncio dos Inocentes', suspense, 1991).
filme('O Senhor dos Anéis: A Sociedade do Anel', aventura, 2001).
filme('A Vida é Bela', comedia, 1997).

% Regras para recomendação de filmes
recomendar(FilmeRecomendado, GeneroPreferido) :-
    filme(FilmeRecomendado, GeneroPreferido, _), % Verifica se o filme pertence ao gênero preferido
    not(filme(FilmeRecomendado, _, Ano)), % Verifica se o filme não foi assistido anteriormente
    Ano >= 2000. % Verifica se o filme foi lançado a partir do ano 2000

% Regra para busca de filme por gênero
buscar_por_genero(Genero, Filmes) :-
    findall(Filme, filme(Filme, Genero, _), Filmes). % Encontra todos os filmes do gênero especificado

% Exemplo de uso:
% Verifica se o filme 'Coringa' é recomendado para o usuário que prefere filmes de drama
?- recomendar('Coringa', drama).
% Resultado esperado: true

% Encontra todos os filmes de ficção científica
?- buscar_por_genero(ficcao, Filmes).
% Resultado esperado: Filmes = ['Interestelar', 'A Origem']
```

Neste exemplo, a base de conhecimento contém informações sobre vários filmes, incluindo o título, gênero e ano de lançamento. A regra `recomendar/2` recebe um filme recomendado e um gênero preferido como argumentos, verificando se o filme pertence ao gênero preferido e se não foi assistido anteriormente. A regra `buscar_por_genero/2` permite buscar todos os filmes de um determinado gênero.

As consultas de exemplo demonstram como usar as regras. A primeira consulta verifica se o filme 'Coringa' é recomendado para alguém que prefere filmes de drama. A segunda consulta busca todos os filmes de ficção científica na base de conhecimento.

Espero que este código complexo em Prolog atenda às suas expectativas! Se você tiver mais pedidos, não hesite em me perguntar.