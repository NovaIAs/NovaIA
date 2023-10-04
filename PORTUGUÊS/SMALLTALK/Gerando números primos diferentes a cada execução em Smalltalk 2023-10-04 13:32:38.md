Claro! Aqui está um código em Smalltalk que gera um padrão de números primos diferentes a cada execução:

```
| primos |
primos := OrderedCollection new.
2 to: 100 do: [:num |
    | isPrimo |
    isPrimo := true.
    2 to: (num sqrt floor) do: [:divisor |
        (num \\ divisor) = 0 ifTrue: [
            isPrimo := false.
            ^ primos
        ]
    ].
    isPrimo ifTrue: [ primos add: num ]
].
primos do: [:primo | Transcript show: primo printString, ' '].
```

Este código começa criando uma coleção vazia chamada `primos` para armazenar os números primos encontrados.

Em seguida, usamos o laço `2 to: 100 do: [:num |]` para iterar de 2 a 100 (você pode ajustar o intervalo conforme necessário). O número 2 é sempre considerado um número primo, então começamos a partir dele.

Dentro do laço, inicializamos a variável `isPrimo` como verdadeira para cada número `num` atual. Em seguida, usamos outro laço `2 to: (num sqrt floor) do: [:divisor |]` para iterar de 2 até a raiz quadrada inteira de `num`. Isso é suficiente para verificar se um número é primo, pois qualquer fator de um número estará no intervalo de 2 a sua raiz quadrada.

Dentro do segundo laço, verificamos se `num` é divisível por `divisor` usando a operação `\\` para obter o resto da divisão. Se o resto for zero, isso significa que `num` não é um número primo, então definimos `isPrimo` como falso e interrompemos a execução do código, retornando a coleção `primos`. Isso é feito usando o operador `^`.

Se `num` passar por todos os divisores sem ser divisível por nenhum deles, isso significa que `num` é um número primo. Nesse caso, adicionamos `num` à coleção `primos` usando o método `add:`.

Finalmente, percorremos a coleção `primos` usando o laço `primos do: [:primo |]` e exibimos cada número primo na janela de transcrição usando o método `show:` e `printString`.

Dessa forma, cada vez que você executar esse código, ele gerará uma lista de números primos diferentes, dependendo do intervalo escolhido.