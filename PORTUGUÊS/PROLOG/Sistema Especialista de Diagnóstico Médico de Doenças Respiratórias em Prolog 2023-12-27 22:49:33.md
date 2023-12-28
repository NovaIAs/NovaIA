Claro! Aqui está um código complexo em Prolog que implementa um sistema especialista para diagnóstico médico de doenças relacionadas ao sistema respiratório. O código utiliza uma base de conhecimento com sintomas e possíveis doenças, e utiliza o algoritmo de encadeamento para frente para realizar o diagnóstico.

```
% Base de conhecimento contendo sintomas e possíveis doenças
sintoma(febre).
sintoma(tosse).
sintoma(dor_peito).
sintoma(falta_ar).

doenca(resfriado_comum) :- sintoma(febre), sintoma(tosse).
doenca(bronquite) :- sintoma(tosse), sintoma(dor_peito).
doenca(asma) :- sintoma(tosse), sintoma(falta_ar).

% Regras para encadeamento para frente
diagnostico(D) :- doenca(D), not(doenca(_)), !.

diagnostico(D) :-
    doenca(D),
    write('O paciente pode ter '), write(D), write('.'),
    nl, write('Recomenda-se procurar um médico para confirmação do diagnóstico.').

diagnostico(_) :-
    write('Não foi possível realizar o diagnóstico. Consulte um médico para mais informações.').

% Exemplo de consulta
:- initialization(main).
main :-
    write('Informe os sintomas do paciente:'), nl,
    write('Febre? (s/n): '), read(Febre),
    write('Tosse? (s/n): '), read(Tosse),
    write('Dor no peito? (s/n): '), read(DorPeito),
    write('Falta de ar? (s/n): '), read(FaltaAr),
    assert(sintoma(febre)) if Febre = 's' else assert(not(sintoma(febre))),
    assert(sintoma(tosse)) if Tosse = 's' else assert(not(sintoma(tosse))),
    assert(sintoma(dor_peito)) if DorPeito = 's' else assert(not(sintoma(dor_peito))),
    assert(sintoma(falta_ar)) if FaltaAr = 's' else assert(not(sintoma(falta_ar))),
    diagnostico(_),
    halt.

:- initialization(main).
```

Explicação do código:
1. A base de conhecimento contém os sintomas relacionados a doenças do sistema respiratório, como febre, tosse, dor no peito e falta de ar.
2. Em seguida, definimos regras para relacionar sintomas a doenças específicas. Por exemplo, se o paciente possui febre e tosse, pode ser um resfriado comum.
3. As regras `diagnostico/1` são responsáveis por fazer o encadeamento para frente e realizar o diagnóstico. A primeira regra verifica se existe uma doença que corresponda aos sintomas fornecidos e retorna essa doença. A segunda regra imprime uma mensagem com o diagnóstico e recomendação para procurar um médico. A terceira regra é ativada quando não foi possível fazer o diagnóstico.
4. O predicado `main/0` é o ponto de entrada do programa. Ele solicita ao usuário que informe os sintomas do paciente e registra-os na base de conhecimento. Em seguida, chama o predicado `diagnostico/1` para realizar o diagnóstico.
5. O código utiliza a diretiva `:- initialization(main)` para garantir que o predicado `main/0` seja executado quando o programa for iniciado.

Esse código é apenas um exemplo de um sistema especialista básico, e pode ser expandido com mais sintomas e doenças, bem como regras mais complexas para diagnóstico.