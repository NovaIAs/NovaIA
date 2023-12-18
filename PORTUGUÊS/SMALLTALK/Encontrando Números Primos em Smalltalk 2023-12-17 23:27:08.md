Claro! Aqui está um exemplo de código em Smalltalk que pode ser considerado grande e diferenciado:

```
| numeros listaPrimos soma |

numeros := Array new: 100000.
listaPrimos := OrderedCollection new.

1 to: numeros size do: [ :i |
    numeros at: i put: i.
].

1 to: numeros size do: [ :i |
    | isPrime |
    isPrime := true.
    
    2 to: (i - 1) do: [ :j |
        (numeros at: i) \\ j = 0 ifTrue: [
            isPrime := false.
            j := i.
        ].
    ].
    
    isPrime ifTrue: [
        listaPrimos add: (numeros at: i).
    ].
].

soma := listaPrimos inject: 0 into: [ :acc :each | acc + each ].

Transcript
    show: 'Lista de números primos: ';
    show: listaPrimos asString;
    cr;
    show: 'Soma dos números primos: ';
    show: soma asString.
```

Neste código, eu criei uma lista de números de 1 a 100000 e uma coleção ordenada para armazenar os números primos encontrados. Em seguida, percorro a lista de números e verifico se cada número é primo ou não. Se for primo, adiciono-o à lista de primos.

Por fim, calculo a soma de todos os números primos encontrados e imprimo a lista de números primos e a soma na janela de saída do Smalltalk usando a classe Transcript.

Este código é um exemplo simples de como encontrar números primos e demonstra o uso de loops, arrays e coleções no Smalltalk.