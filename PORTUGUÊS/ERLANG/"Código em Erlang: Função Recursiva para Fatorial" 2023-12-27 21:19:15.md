Claro! Aqui está um código complexo em Erlang, que usa uma função recursiva para calcular o fatorial de um número:

```erlang
-module(fatorial).
-export([fatorial/1]).

fatorial(0) -> 1;
fatorial(N) -> N * fatorial(N-1).
```

Neste código, definimos um módulo chamado "fatorial" que exporta a função "fatorial/1". A função fatorial recebe um argumento N e retorna o fatorial desse número.

A primeira linha do código define o caso base da função recursiva, onde se N for igual a 0, retornamos 1, pois o fatorial de 0 é 1.

A segunda linha é a parte recursiva da função. Se N não for igual a 0, multiplicamos N pelo resultado da chamada recursiva da função fatorial(N-1). Ou seja, calculamos o fatorial de N-1 e multiplicamos pelo próprio N.

Essa recursão continua até que N seja igual a 0, momento em que a função retorna 1 e a recursão é encerrada.

Esse código em Erlang é um exemplo simples, mas demonstra a utilização de recursão para calcular o fatorial de um número. Erlang é uma linguagem funcional projetada para escalabilidade e concorrência, e a recursão é uma técnica comumente utilizada nessa linguagem.