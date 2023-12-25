Claro! Aqui está um código em Smalltalk que gera uma lista de números primos usando o Crivo de Eratóstenes.

```
Object subclass: #CrivoDeEratostenes
    instanceVariableNames: 'limite numerosPrimos'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'CrivoDeEratostenes'

CrivoDeEratostenes>>initialize: aLimite
    super initialize.
    limite := aLimite.
    numerosPrimos := OrderedCollection new.

CrivoDeEratostenes>>gerarNumerosPrimos
    numerosPrimos removeAll.
    | primos |
    primos := Array new: limite + 1.
    2 to: limite do: [:i |
        primos at: i put: true].
    2 to: (limite sqrt asInteger) do: [:i |
        (primos at: i) ifTrue: [
            (i*i) to: limite by: i do: [:j |
                primos at: j put: false]]].
    2 to: limite do: [:i |
        (primos at: i) ifTrue: [numerosPrimos add: i]].

CrivoDeEratostenes>>exibirNumerosPrimos
    numerosPrimos do: [:numero |
        Transcript show: numero printString; cr].

"Uso do CrivoDeEratostenes"
| crivo |
crivo := CrivoDeEratostenes new initialize: 100.
crivo gerarNumerosPrimos.
crivo exibirNumerosPrimos.
```

Neste código, definimos uma classe chamada `CrivoDeEratostenes` que tem dois atributos: `limite`, que define o limite superior para a geração dos números primos, e `numerosPrimos`, uma coleção ordenada que armazena os números primos encontrados.

No método `gerarNumerosPrimos`, implementamos o algoritmo do Crivo de Eratóstenes. Criamos um array `primos` de tamanho `limite + 1`, onde inicializamos todos os elementos como `true`. Em seguida, percorremos os números de 2 até a raiz quadrada do limite, marcando como `false` os números múltiplos dos números primos encontrados. Por fim, percorremos novamente o array `primos`, adicionando os números primos encontrados à coleção `numerosPrimos`.

O método `exibirNumerosPrimos` simplesmente exibe os números primos encontrados na saída do Transcript.

No exemplo de uso do `CrivoDeEratostenes`, criamos uma instância da classe com um limite de 100, geramos os números primos chamando o método `gerarNumerosPrimos` e, em seguida, exibimos os números primos encontrados com o método `exibirNumerosPrimos`.