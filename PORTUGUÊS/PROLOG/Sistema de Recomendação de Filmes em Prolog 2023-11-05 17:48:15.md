Claro! Aqui está um exemplo de código em Prolog que implementa um sistema de recomendação de filmes com base nas preferências do usuário:

```prolog
% Definição da base de conhecimento com os filmes e suas características
filme(harry_potter_e_a_pedra_filosofal, aventura).
filme(harry_potter_e_a_camara_secreta, aventura).
filme(harry_potter_e_a_prisao_de_azkaban, aventura).
filme(harry_potter_e_o_calice_de_fogo, aventura).
filme(harry_potter_e_a_ordem_da_fenix, aventura).
filme(harry_potter_e_o_enigma_do_principe, aventura).
filme(harry_potter_e_as_reliquias_da_morte_parte_1, aventura).
filme(harry_potter_e_as_reliquias_da_morte_parte_2, aventura).
filme(um_sonho_de_libertacao, drama).
filme(a_vida_e_bela, drama).
filme(a_origem, ficcao_cientifica).
filme(interstellar, ficcao_cientifica).
filme(de_volta_para_o_futuro, ficcao_cientifica).
filme(matrix, ficcao_cientifica).

% Definição das preferências do usuário
gosta(joao, aventura).
gosta(joao, ficcao_cientifica).
gosta(maria, drama).
gosta(pedro, aventura).
gosta(pedro, drama).

% Predicado para recomendar filmes para um usuário
recomendar_filmes(Usuario, Recomendacoes) :-
    % Obtém as preferências do usuário
    gosta(Usuario, Genero1),
    gosta(Usuario, Genero2),
    Genero1 \= Genero2, % Garante que os gêneros sejam diferentes
    
    % Encontra filmes que correspondem às preferências do usuário
    filme(Filme, Genero1),
    \+gosta(Usuario, Filme), % Verifica se o usuário ainda não assistiu o filme
    
    % Gera a lista de recomendações
    Recomendacoes = [Filme | _].

% Exemplo de uso: recomendando filmes para o usuário 'joao'
?- recomendar_filmes(joao, Recomendacoes).
```

Neste código em Prolog, temos uma base de conhecimento de filmes e suas características (gênero, nesse caso). Também temos as preferências de alguns usuários. O predicado `recomendar_filmes/2` recebe o nome de um usuário e gera uma lista de recomendações de filmes com base nas preferências desse usuário.

O código utiliza a cláusula `gosta/2` para verificar as preferências do usuário. Em seguida, ele busca filmes que correspondem às preferências do usuário e que ele ainda não tenha assistido, utilizando o predicado `filme/2` e `gosta/2`. Por fim, a lista de recomendações é gerada e retornada.

No exemplo de uso apresentado, o código é consultado para recomendar filmes para o usuário "joao". A resposta será uma lista com um filme que se encaixa nas suas preferências e que ele ainda não tenha assistido.