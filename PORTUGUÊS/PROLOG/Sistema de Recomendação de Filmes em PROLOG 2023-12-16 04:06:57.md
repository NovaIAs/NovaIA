Claro! Aqui está um exemplo de código em PROLOG que implementa um sistema de recomendação de filmes baseado em preferências dos usuários:

```prolog
% Base de conhecimento com filmes e suas categorias
filme('Interestelar', ['Ficção Científica', 'Aventura', 'Drama']).
filme('Matrix', ['Ficção Científica', 'Ação']).
filme('O Poderoso Chefão', ['Drama', 'Crime']).
filme('De Volta para o Futuro', ['Ficção Científica', 'Aventura', 'Comédia']).
filme('Clube da Luta', ['Drama', 'Ação']).
% Adicione mais filmes e categorias à base de conhecimento

% Base de conhecimento com preferências dos usuários
preferencia(joao, ['Ficção Científica', 'Aventura']).
preferencia(maria, ['Drama', 'Romance']).
preferencia(pedro, ['Ação', 'Comédia']).
% Adicione mais usuários e suas preferências à base de conhecimento

% Predicado para encontrar filmes recomendados para um usuário
recomendar_filme(Usuario, FilmeRecomendado) :-
    preferencia(Usuario, Preferencias),
    filme(FilmeRecomendado, Categorias),
    intersect(Preferencias, Categorias, FilmesEmComum),
    length(FilmesEmComum, NumFilmesEmComum),
    NumFilmesEmComum > 0.

% Predicado para encontrar todas as recomendações para um usuário
recomendar_filmes(Usuario, FilmesRecomendados) :-
    findall(Filme, recomendar_filme(Usuario, Filme), FilmesRecomendados).

% Exemplo de uso:
% ?- recomendar_filmes(joao, Filmes).
% Filmes = ['Interestelar', 'Matrix', 'De Volta para o Futuro'].
```

Neste código, temos uma base de conhecimento que contém filmes e suas categorias, bem como as preferências dos usuários. Através dos predicados `recomendar_filme/2` e `recomendar_filmes/2`, podemos encontrar filmes recomendados para um usuário específico com base em suas preferências.

O predicado `recomendar_filme/2` recebe um usuário e um filme como parâmetros e verifica se o usuário possui preferências em comum com o filme, utilizando a função `intersect/3` para encontrar as categorias em comum. Em seguida, verifica se há pelo menos um filme em comum e retorna verdadeiro caso haja.

O predicado `recomendar_filmes/2` recebe um usuário como parâmetro e utiliza o predicado `findall/3` para encontrar todos os filmes recomendados para esse usuário, retornando-os em uma lista.

No exemplo de uso mostrado no final do código, é possível ver um exemplo de como encontrar os filmes recomendados para o usuário "joao", que possui preferências em "Ficção Científica" e "Aventura".