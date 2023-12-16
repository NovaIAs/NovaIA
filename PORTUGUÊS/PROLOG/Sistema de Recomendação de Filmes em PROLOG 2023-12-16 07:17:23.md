Claro! Aqui está um exemplo de código PROLOG complexo que apresenta um sistema de recomendador de filmes baseado em preferências do usuário:

```prolog
% Base de conhecimento contendo informações sobre os filmes
filme('O Poderoso Chefão', ['Drama', 'Crime'], 1972, 9.2).
filme('O Senhor dos Anéis: O Retorno do Rei', ['Aventura', 'Fantasia'], 2003, 8.9).
filme('Interestelar', ['Ficção Científica', 'Drama'], 2014, 8.6).
filme('A Origem', ['Ação', 'Ficção Científica'], 2010, 8.8).
filme('Clube da Luta', ['Drama', 'Ação'], 1999, 8.8).
filme('Pulp Fiction: Tempo de Violência', ['Crime', 'Drama'], 1994, 8.9).
filme('Matrix', ['Ação', 'Ficção Científica'], 1999, 8.7).
filme('Vingadores: Ultimato', ['Ação', 'Aventura'], 2019, 8.4).
% Adicione mais filmes à base de conhecimento, se desejar.

% Regras para recomendar filmes com base nas preferências do usuário
recomendar(FilmeRecomendado) :-
    escrever_saudacao,
    escrever_pergunta_genero(Genero),
    escrever_pergunta_ano(Ano),
    escrever_pergunta_avaliacao(Avaliacao),
    verificar_filmes(Genero, Ano, Avaliacao, Filmes),
    selecionar_filme(Filmes, FilmeRecomendado),
    escrever_recomendacao(FilmeRecomendado).

% Regras auxiliares para interagir com o usuário e realizar a recomendação
escrever_saudacao :-
    write('Bem-vindo ao sistema de recomendação de filmes!'), nl.

escrever_pergunta_genero(Genero) :-
    write('Qual gênero de filme você prefere?'), nl,
    write('Opções disponíveis: Drama, Crime, Aventura, Fantasia, Ficção Científica, Ação.'), nl,
    read(Genero).

escrever_pergunta_ano(Ano) :-
    write('Em que ano você gostaria que o filme tenha sido lançado?'), nl,
    read(Ano).

escrever_pergunta_avaliacao(Avaliacao) :-
    write('Qual a avaliação mínima que você espera para o filme?'), nl,
    write('Digite um número entre 0 e 10.'), nl,
    read(Avaliacao).

verificar_filmes(Genero, Ano, Avaliacao, Filmes) :-
    findall(NomeFilme, (
        filme(NomeFilme, Generos, AnoFilme, AvaliacaoFilme),
        member(Genero, Generos),
        AnoFilme >= Ano,
        AvaliacaoFilme >= Avaliacao
    ), Filmes).

selecionar_filme(Filmes, FilmeRecomendado) :-
    length(Filmes, NumeroFilmes),
    random(0, NumeroFilmes, Indice),
    nth0(Indice, Filmes, FilmeRecomendado).

escrever_recomendacao(FilmeRecomendado) :-
    write('Eu recomendo assistir ao filme: '), write(FilmeRecomendado), write('!'), nl.
```

Explicação do Código:

- Primeiramente, definimos uma base de conhecimento que contém informações sobre filmes, como nome, gênero, ano de lançamento e avaliação.
- Em seguida, temos a regra `recomendar/1`, que é o ponto de entrada para o sistema de recomendação de filmes. Essa regra inicia a interação com o usuário, fazendo perguntas sobre as preferências do usuário e, em seguida, recomenda um filme com base nas respostas fornecidas.
- As regras auxiliares, como `escrever_saudacao/0`, `escrever_pergunta_genero/1`, `escrever_pergunta_ano/1` e `escrever_pergunta_avaliacao/1`, são responsáveis por exibir mensagens ao usuário e ler suas respostas.
- A regra `verificar_filmes/4` verifica todos os filmes que atendem aos critérios de gênero, ano de lançamento e avaliação mínima fornecidos pelo usuário.
- A regra `selecionar_filme/2` seleciona aleatoriamente um filme da lista de filmes que correspondem aos critérios fornecidos pelo usuário.
- Por fim, a regra `escrever_recomendacao/1` exibe a recomendação final ao usuário, indicando o filme que foi selecionado.

Esse código é um exemplo simplificado de um sistema de recomendação de filmes em PROLOG. É possível modificar e expandir esse código de acordo com as necessidades e requisitos específicos do projeto.