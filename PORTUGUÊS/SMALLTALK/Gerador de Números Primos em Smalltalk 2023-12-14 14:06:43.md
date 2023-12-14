Claro! Aqui está um código em Smalltalk que gera uma sequência de números primos e imprime os números na tela.

Primeiro, vamos criar uma classe chamada "GeradorDePrimos" que será responsável por gerar os números primos:

```smalltalk
Object subclass: #GeradorDePrimos
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'GeradorDePrimos'
```

Agora, vamos adicionar um método de classe chamado "ehPrimo:" que verifica se um número é primo:

```smalltalk
GeradorDePrimos class >> ehPrimo: numero
    | i |
    i := 2.
    [i <= numero / 2] whileTrue: [
        (numero \\ i) = 0 ifTrue: [^ false].
        i := i + 1].
    ^ true
```

Em seguida, vamos adicionar um método de classe chamado "gerarPrimosAte:" que gera todos os números primos menores ou iguais a um determinado número:

```smalltalk
GeradorDePrimos class >> gerarPrimosAte: limite
    | primos |
    primos := OrderedCollection new.
    2 to: limite do: [:numero |
        (self ehPrimo: numero) ifTrue: [primos add: numero]].
    ^ primos
```

Agora, podemos usar a classe "GeradorDePrimos" para gerar e imprimir os números primos. Vamos criar uma instância da classe, definir o limite e chamar o método "gerarPrimosAte:" para obter a lista de números primos:

```smalltalk
| gerador primos limite |
gerador := GeradorDePrimos new.
limite := 1000. "Defina o limite conforme necessário"
primos := gerador gerarPrimosAte: limite.
primos do: [:numero |
    Transcript show: numero printString; cr].
```

Neste exemplo, o código irá gerar e imprimir todos os números primos menores ou iguais a 1000. Você pode ajustar o limite conforme necessário.

Espero que este código em Smalltalk atenda às suas expectativas!