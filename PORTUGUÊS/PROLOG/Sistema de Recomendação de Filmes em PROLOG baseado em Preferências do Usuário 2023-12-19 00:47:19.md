Claro! Aqui está um exemplo de código em PROLOG que implementa um sistema de recomendação de filmes baseado em preferências do usuário.

```prolog
% Fatos - filmes e suas categorias
filme('O Poderoso Chefão', 'crime').
filme('Interestelar', 'ficção científica').
filme('Clube da Luta', 'drama').
filme('Pulp Fiction', 'crime').
filme('Matrix', 'ação').

% Fatos - preferências do usuário
preferencia(joao, 'drama').
preferencia(joao, 'crime').
preferencia(maria, 'ficção científica').
preferencia(maria, 'ação').
preferencia(pedro, 'drama').
preferencia(pedro, 'ação').

% Regra - encontrar filmes recomendados
recomendacao(Usuario, Recomendacao) :-
    preferencia(Usuario, Categoria),
    filme(Recomendacao, Categoria),
    \+ assistiu(Usuario, Recomendacao).

% Fato - filmes já assistidos
assistiu(joao, 'O Poderoso Chefão').
assistiu(joao, 'Matrix').
assistiu(maria, 'Interestelar').
assistiu(pedro, 'Clube da Luta').

% Exemplo de uso
?- recomendacao(joao, Recomendacao).
```

Neste exemplo, temos uma base de conhecimento que contém informações sobre filmes e suas categorias, bem como as preferências do usuário. A regra `recomendacao/2` é usada para encontrar filmes recomendados para um determinado usuário. Ela verifica a preferência do usuário, encontra filmes com a mesma categoria e, em seguida, verifica se o usuário já assistiu a esse filme.

No exemplo de uso, estamos buscando recomendações para o usuário "joao". A resposta será uma lista de filmes que ainda não foram assistidos por ele, mas que se enquadram nas suas preferências de "drama" e "crime".