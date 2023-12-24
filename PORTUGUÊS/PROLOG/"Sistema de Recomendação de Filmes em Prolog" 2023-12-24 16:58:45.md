Claro! Aqui está um exemplo de código em Prolog que implementa um sistema de recomendação de filmes com base nas preferências do usuário:

```prolog
% Base de conhecimento com informações sobre os filmes
filme("Interestelar", ["Ficção Científica", "Aventura", "Drama"], 2014, 8.6).
filme("O Poderoso Chefão", ["Crime", "Drama"], 1972, 9.2).
filme("Matrix", ["Ação", "Ficção Científica"], 1999, 8.7).
filme("O Senhor dos Anéis: A Sociedade do Anel", ["Aventura", "Fantasia"], 2001, 8.8).
filme("Clube da Luta", ["Drama", "Thriller"], 1999, 8.8).
filme("Vingadores: Ultimato", ["Ação", "Aventura", "Fantasia"], 2019, 8.4).

% Predicado para recomendar filmes com base nas preferências do usuário
recomendar_filme(GenerosPreferidos, AnoMinimo, AvaliacaoMinima, FilmeRecomendado) :-
    filme(FilmeRecomendado, Generos, AnoLancamento, Avaliacao),
    diferenca_generos(GenerosPreferidos, Generos),
    AnoLancamento >= AnoMinimo,
    Avaliacao >= AvaliacaoMinima.

% Predicado para verificar a diferença entre duas listas de gêneros
diferenca_generos([], _).
diferenca_generos([Genero|Resto], Generos) :-
    \+ member(Genero, Generos),
    diferenca_generos(Resto, Generos).

% Exemplo de uso:
:- initialization(main).

main :-
    write("Bem-vindo ao sistema de recomendação de filmes!"), nl,
    write("Por favor, digite seus gêneros preferidos (separados por vírgula): "),
    read(GenerosPreferidos),
    write("Digite o ano mínimo de lançamento: "),
    read(AnoMinimo),
    write("Digite a avaliação mínima (de 0 a 10): "),
    read(AvaliacaoMinima),
    recomendar_filme(GenerosPreferidos, AnoMinimo, AvaliacaoMinima, FilmeRecomendado),
    write("Recomendamos o filme "), write(FilmeRecomendado), write("!"), nl.
```

Explicação do código:

1. A base de conhecimento contém informações sobre alguns filmes, como nome, gêneros, ano de lançamento e avaliação.
2. O predicado `recomendar_filme` recebe como parâmetros os gêneros preferidos do usuário, o ano mínimo de lançamento e a avaliação mínima desejada.
3. O predicado verifica se existe algum filme na base de conhecimento que atenda aos critérios de recomendação.
4. O predicado `diferenca_generos` é utilizado para verificar a diferença entre os gêneros preferidos do usuário e os gêneros de um filme. Isso é importante para garantir que o filme recomendado não contenha nenhum gênero que o usuário não goste.
5. No exemplo de uso, o programa solicita ao usuário que digite suas preferências, o ano mínimo de lançamento e a avaliação mínima desejada. Em seguida, o programa recomenda um filme com base nessas informações.

Lembre-se de que este é apenas um exemplo e você pode adaptar o código às suas necessidades.