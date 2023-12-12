Aqui está um exemplo de código em Prolog que simula um jogo de adivinhação em português:

```prolog
%% Base de fatos contendo os possíveis animais
animal(leao).
animal(tigre).
animal(zebra).
animal(girafa).
animal(macaco).

%% Base de fatos contendo as características dos animais
caracteristica(leao, carnivoro).
caracteristica(tigre, carnivoro).
caracteristica(zebra, herbivoro).
caracteristica(girafa, herbivoro).
caracteristica(macaco, onivoro).

%% Predicado para adivinhar o animal
adivinhar_animal :-
    write('Pense em um animal e responda as perguntas abaixo:'), nl,
    write('O animal e carnivoro? (s/n)'), nl,
    read(Resposta1),
    (Resposta1 = s ->
        write('O animal e herbivoro? (s/n)'), nl,
        read(Resposta2),
        (Resposta2 = s ->
            write('O animal e um(a) zebra.'), nl
        ;   write('O animal e um(a) girafa.'), nl
        )
    ;   write('O animal e onivoro? (s/n)'), nl,
        read(Resposta3),
        (Resposta3 = s ->
            write('O animal e um(a) macaco.'), nl
        ;   write('O animal e um(a) leao ou um(a) tigre.'), nl
        )
    ).

%% Chamar o predicado para iniciar o jogo de adivinhação
:- adivinhar_animal.
```

Neste código, temos uma base de fatos que contém os possíveis animais e suas características. Em seguida, o predicado `adivinhar_animal` é chamado para iniciar o jogo de adivinhação.

O jogo começa com uma pergunta sobre a característica do animal ser carnivoro. Dependendo da resposta (s para sim, n para não), o código faz uma nova pergunta sobre a característica ser herbivoro ou onivoro. Com base nas respostas, o código deduz qual animal o usuário está pensando e exibe o resultado.

Por exemplo, se o usuário responder "s" para a primeira pergunta e "n" para a segunda pergunta, o código vai deduzir que o animal é um leão ou um tigre.

Esse código é apenas um exemplo simples de jogo de adivinhação em Prolog, mas pode ser expandido para incluir mais animais e características, tornando-o mais complexo e diferenciado.