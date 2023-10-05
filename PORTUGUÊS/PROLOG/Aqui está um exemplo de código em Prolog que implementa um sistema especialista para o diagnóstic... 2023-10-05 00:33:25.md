Claro! Aqui está um exemplo de código em Prolog que implementa um sistema especialista para o diagnóstico de doenças. O sistema é baseado em regras e utiliza fatos e inferências para chegar a um diagnóstico final.

```
% Fatos sobre as doenças
doenca(resfriado_comum) :- sintoma(febre).
doenca(resfriado_comum) :- sintoma(nariz_escorrendo).
doenca(resfriado_comum) :- sintoma(espirros).

doenca(gripe) :- sintoma(febre_alta).
doenca(gripe) :- sintoma(dor_no_corpo).
doenca(gripe) :- sintoma(tosse_seca).

doenca(alergia) :- sintoma(coceira_nos_olhos).
doenca(alergia) :- sintoma(espirros).
doenca(alergia) :- sintoma(dificuldade_respiratoria).

% Regras para o diagnóstico
diagnostico(Doenca) :- sintoma(Sintoma),
                      doenca(Doenca),
                      not(exclui_doenca(Doenca, Sintoma)).

% Regras para exclusão de doenças
exclui_doenca(resfriado_comum, febre_alta).
exclui_doenca(gripe, nariz_escorrendo).
exclui_doenca(alergia, dor_no_corpo).

% Perguntas ao usuário
pergunta(Sintoma) :- write('Você está sentindo '), write(Sintoma), write('? (s/n) '),
                     read(Resposta), nl,
                     (Resposta == 's' -> assert(sintoma(Sintoma)) ; assert(not(sintoma(Sintoma)))).

% Predicado principal para iniciar o diagnóstico
iniciar_diagnostico :- write('Responda as perguntas abaixo para obter um diagnóstico.'), nl,
                      pergunta(febre),
                      pergunta(nariz_escorrendo),
                      pergunta(espirros),
                      pergunta(febre_alta),
                      pergunta(dor_no_corpo),
                      pergunta(tosse_seca),
                      pergunta(coceira_nos_olhos),
                      pergunta(dificuldade_respiratoria),
                      nl, write('Realizando o diagnóstico...'), nl,
                      findall(Doenca, diagnostico(Doenca), Doencas),
                      nl, write('O diagnóstico sugere as seguintes doenças:'), nl,
                      write(Doencas), nl.

% Executa o diagnóstico
:- iniciar_diagnostico.
```

Neste código, o sistema especialista faz perguntas ao usuário sobre os sintomas que ele está sentindo. Com base nas respostas dadas, o sistema utiliza regras e fatos para inferir qual doença pode estar causando esses sintomas. Em seguida, o sistema apresenta o diagnóstico final, sugerindo as doenças que podem estar relacionadas aos sintomas informados.

As doenças (resfriado_comum, gripe, alergia) são representadas como fatos, enquanto as regras de diagnóstico são definidas utilizando o predicado `diagnostico/1`. O sistema também possui regras para excluir doenças com base em sintomas específicos.

O programa é executado chamando o predicado `iniciar_diagnostico/0`, que inicia o processo de diagnóstico, fazendo as perguntas ao usuário e exibindo o diagnóstico final.

Este código é apenas um exemplo de como um sistema especialista pode ser implementado em Prolog. Você pode personalizar e expandir as regras e fatos para se adequar a diferentes cenários e doenças.