Claro! Aqui está um exemplo de código em Smalltalk:

```
| listaNumeros listaPares listaImpares |
listaNumeros := #(1 2 3 4 5 6 7 8 9 10).
listaPares := OrderedCollection new.
listaImpares := OrderedCollection new.

listaNumeros do: [:numero |
    (numero \\ 2) = 0 ifTrue: [
        listaPares add: numero
    ] ifFalse: [
        listaImpares add: numero
    ]
].

Transcript show: 'Lista de números pares: '.
listaPares do: [:numero |
    Transcript show: numero printString; space
].

Transcript cr.

Transcript show: 'Lista de números ímpares: '.
listaImpares do: [:numero |
    Transcript show: numero printString; space
].
```

Vamos explicar o código:
- Na primeira linha, declaramos três variáveis: `listaNumeros`, `listaPares` e `listaImpares`.
- Na segunda linha, inicializamos a variável `listaNumeros` com um array contendo os números de 1 a 10.
- Na terceira linha, inicializamos as variáveis `listaPares` e `listaImpares` como coleções vazias.
- A partir da quarta linha, usamos o método `do:` para percorrer cada elemento da lista de números.
- Dentro do bloco do método `do:`, verificamos se o número é par ou ímpar utilizando a operação `%` (resto da divisão) e o operador `ifTrue:ifFalse:` para adicionar o número na lista correspondente.
- Após o loop, utilizamos o objeto `Transcript` para exibir as duas listas separadamente.
- Na linha 16, mostramos no Transcript a mensagem "Lista de números pares: " e, em seguida, percorremos a lista de números pares, exibindo cada um deles.
- Na linha 18, usamos o método `cr` para pular uma linha no Transcript.
- Na linha 20, mostramos no Transcript a mensagem "Lista de números ímpares: " e, em seguida, percorremos a lista de números ímpares, exibindo cada um deles.

Esse código em Smalltalk recebe uma lista de números e separa-os em duas listas diferentes: uma contendo os números pares e outra contendo os números ímpares. Em seguida, exibe essas duas listas no Transcript.