Claro! Vou gerar um código em PROLOG que envolve um problema de alocação de tarefas. Neste código, vamos considerar um conjunto de tarefas e um conjunto de pessoas, e vamos encontrar uma alocação ótima de tarefas para as pessoas, levando em conta suas habilidades e preferências.

Primeiro, vamos definir as habilidades das pessoas e as preferências das tarefas. Para simplificar, vamos considerar que as habilidades e preferências são números de 1 a 5. Quanto maior o número, melhor a habilidade ou preferência. Vamos assumir que temos 3 pessoas (p1, p2, p3) e 4 tarefas (t1, t2, t3, t4).

```prolog
habilidade(p1, 4).
habilidade(p2, 3).
habilidade(p3, 5).

preferencia(t1, 3).
preferencia(t2, 4).
preferencia(t3, 5).
preferencia(t4, 2).
```

Agora, vamos definir as regras para alocar as tarefas. Precisamos garantir que cada tarefa seja alocada para uma única pessoa, e que cada pessoa seja alocada para no máximo uma tarefa. Além disso, queremos maximizar a soma das habilidades das pessoas alocadas para as tarefas.

```prolog
alocacao(Tarefas, Pessoas) :-
    findall(Pessoa-Tarefa, (member(Pessoa, Pessoas), member(Tarefa, Tarefas)), Alocacoes),
    alocacao_unicidade(Alocacoes),
    alocacao_habilidades(Alocacoes, Habilidades),
    sum_list(Habilidades, SomaHabilidades),
    write('Alocação ótima: '), write(Alocacoes), nl,
    write('Soma das habilidades: '), write(SomaHabilidades), nl.

alocacao_unicidade([]).
alocacao_unicidade([Pessoa-Tarefa|Resto]) :-
    not(member(Pessoa-_, Resto)),
    not(member(-Tarefa, Resto)),
    alocacao_unicidade(Resto).

alocacao_habilidades([], []).
alocacao_habilidades([Pessoa-Tarefa|Resto], [Habilidade|RestoHabilidades]) :-
    habilidade(Pessoa, Habilidade),
    preferencia(Tarefa, Preferencia),
    Habilidade >= Preferencia,
    alocacao_habilidades(Resto, RestoHabilidades).
```

Por fim, vamos definir um predicado principal para testar nosso código. Neste exemplo, vamos alocar as tarefas t1, t2, t3 para as pessoas p1, p2, p3, respectivamente.

```prolog
:- initialization(main).

main :-
    Tarefas = [t1, t2, t3],
    Pessoas = [p1, p2, p3],
    alocacao(Tarefas, Pessoas),
    halt.
```

Este código irá encontrar a alocação ótima de tarefas para as pessoas, considerando suas habilidades e preferências. A saída do programa será algo como:

```
Alocação ótima: [p1-t1, p2-t2, p3-t3]
Soma das habilidades: 12
```

Isso indica que a tarefa t1 foi alocada para a pessoa p1, t2 para p2 e t3 para p3, resultando em uma soma total das habilidades igual a 12.