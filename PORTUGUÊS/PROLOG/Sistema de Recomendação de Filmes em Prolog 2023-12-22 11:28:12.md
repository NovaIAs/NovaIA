Claro, vou gerar um código em Prolog para você. Este código é um exemplo de um sistema de recomendação de filmes. Ele utiliza uma base de dados de filmes com diferentes atributos para recomendar filmes semelhantes com base nas preferências do usuário.

```
% Base de dados de filmes
filme('O Poderoso Chefão', ['Crime', 'Drama'], 9.2, 1972).
filme('Cidade de Deus', ['Crime', 'Drama'], 8.6, 2002).
filme('Interestelar', ['Aventura', 'Ficção Científica'], 8.6, 2014).
filme('O Senhor dos Anéis: A Sociedade do Anel', ['Aventura', 'Fantasia'], 8.8, 2001).
filme('Clube da Luta', ['Drama'], 8.8, 1999).
filme('Pulp Fiction', ['Crime', 'Drama'], 8.9, 1994).
filme('A Origem', ['Ação', 'Aventura', 'Ficção Científica'], 8.8, 2010).

% Regras para recomendar filmes
recomendar(FilmeRecomendado, Generos, AvaliacaoMinima) :-
    filme(FilmeRecomendado, GenerosFilme, AvaliacaoFilme, _),
    avaliacao_satisfatoria(AvaliacaoFilme, AvaliacaoMinima),
    generos_satisfatorios(GenerosFilme, Generos).

avaliacao_satisfatoria(AvaliacaoFilme, AvaliacaoMinima) :-
    AvaliacaoFilme >= AvaliacaoMinima.

generos_satisfatorios(GenerosFilme, Generos) :-
    intersection(GenerosFilme, Generos, GenerosComuns),
    length(GenerosComuns, NumGenerosComuns),
    length(GenerosFilme, NumGenerosFilme),
    NumGenerosComuns == NumGenerosFilme.

% Exemplo de uso do sistema de recomendação de filmes
?- recomendar(Filme, ['Crime', 'Drama'], 8.5).
```

Explicação do código:

1. Primeiro, definimos uma base de dados de filmes, onde cada filme possui um título, uma lista de gêneros, uma avaliação e um ano de lançamento.

2. Em seguida, definimos uma regra chamada `recomendar/3` que recebe os parâmetros `FilmeRecomendado`, `Generos` e `AvaliacaoMinima`. Essa regra é responsável por recomendar um filme com base nos critérios fornecidos.

3. Dentro da regra `recomendar/3`, utilizamos a cláusula `filme/4` para obter informações sobre cada filme na base de dados. Em seguida, verificamos se a avaliação do filme e os gêneros estão de acordo com os critérios fornecidos.

4. As regras `avaliacao_satisfatoria/2` e `generos_satisfatorios/2` são responsáveis por verificar se a avaliação do filme e os gêneros estão dentro dos critérios definidos.

5. Por fim, o exemplo de uso do sistema de recomendação é mostrado. Neste caso, estamos solicitando um filme com gêneros "Crime" e "Drama" e uma avaliação mínima de 8.5.

Espero que este exemplo de código em Prolog ajude a entender como implementar um sistema de recomendação de filmes.