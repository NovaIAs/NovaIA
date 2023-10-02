Claro, aqui está um código em Smalltalk que gera uma sequência de números primos em ordem crescente:

```smalltalk
gerarSequenciaPrimos: n
    | numerosPrimos |
    numerosPrimos := OrderedCollection new.
    (2 to: n) do: [:i |
        (2 to: i - 1) do: [:j |
            (i \\ j) = 0 ifTrue: [^numerosPrimos]].
        numerosPrimos add: i].
    ^numerosPrimos

sequenciaPrimos := self gerarSequenciaPrimos: 100.
sequenciaPrimos do: [:numero | Transcript show: numero printString; cr].
```

Neste código, a função `gerarSequenciaPrimos` recebe um número `n` como parâmetro e retorna uma coleção ordenada contendo todos os números primos até `n`. O algoritmo utiliza um laço `do:` para iterar através dos números de 2 a `n`, e dentro desse laço, outro laço `do:` é utilizado para verificar se o número atual é divisível por qualquer número menor que ele. Se o número for divisível, ele não é primo e o laço é interrompido. Caso contrário, o número é adicionado à coleção `numerosPrimos`.

Em seguida, é criada uma sequência de números primos até 100 utilizando a função `gerarSequenciaPrimos`, e cada número da sequência é exibido no console utilizando `Transcript show:`.