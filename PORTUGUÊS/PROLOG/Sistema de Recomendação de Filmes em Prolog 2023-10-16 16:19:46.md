Claro! Aqui está um exemplo de um código complexo em Prolog que implementa um sistema de recomendação de filmes baseado em preferências do usuário. O programa usa uma base de dados de filmes e características para fazer as recomendações.

```prolog
% Base de dados de filmes e características
filme('Vingadores: Ultimato', [acao, ficcao, superheroi], 2019).
filme('Interestelar', [ficcao, drama], 2014).
filme('O Poderoso Chefão', [drama], 1972).
filme('A Origem', [ficcao, suspense], 2010).
filme('Pulp Fiction', [drama, crime], 1994).
filme('O Senhor dos Anéis: O Retorno do Rei', [fantasia, aventura], 2003).
% ... adicione mais filmes e características aqui

% Predicado para recomendar filmes com base nas preferências do usuário
recomendar_filme(GenerosPreferidos, AnoPreferido, FilmeRecomendado) :-
    filme(FilmeRecomendado, Generos, Ano),
    subset(GenerosPreferidos, Generos),
    Ano >= AnoPreferido.

% Predicado para verificar se uma lista está contida em outra
subset([], _).
subset([H|T], Set) :-
    member(H, Set),
    subset(T, Set).

% Exemplo de uso
:- initialization(main).
main :-
    write('Digite seus gêneros preferidos separados por vírgula: '),
    read(GenerosPreferidos),
    write('Digite o ano preferido: '),
    read(AnoPreferido),
    recomendar_filme(GenerosPreferidos, AnoPreferido, FilmeRecomendado),
    write('Recomendação: '),
    write(FilmeRecomendado),
    halt.
```

Explicação do código:
1. A base de dados contém informações sobre filmes, incluindo o título, os gêneros e o ano de lançamento.
2. O predicado `recomendar_filme/3` recebe as preferências do usuário (uma lista de gêneros e um ano) e retorna um filme recomendado que corresponde às preferências.
3. O predicado `subset/2` é usado para verificar se uma lista está contida em outra.
4. No exemplo de uso, o programa solicita ao usuário que digite seus gêneros preferidos e o ano preferido.
5. Em seguida, chama o predicado `recomendar_filme/3` com as preferências informadas pelo usuário e armazena o resultado na variável `FilmeRecomendado`.
6. Por fim, exibe a recomendação para o usuário.

Esse código é apenas um exemplo de como um sistema de recomendação de filmes pode ser implementado em Prolog. É possível adicionar mais filmes à base de dados e expandir a lógica para incluir outros critérios de recomendação, como classificação etária ou avaliações dos usuários.