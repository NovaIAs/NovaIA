Claro! Aqui está um exemplo de código em Smalltalk que cria uma lista de números primos e depois os imprime na tela:

```smalltalk
Object subclass: Primes
    instanceVariableNames: 'primes'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Primes'

Primes >> initialize
    super initialize.
    primes := OrderedCollection new.

Primes >> isPrime: n
    | limit |
    limit := n sqrt asInteger.
    primes do: [:each | (each > limit) ifTrue: [^ true]. (n \\ each = 0) ifTrue: [^ false]].
    ^ true

Primes >> generatePrimes: count
    | i |
    i := 2.
    [primes size < count] whileTrue: [
        (self isPrime: i) ifTrue: [primes add: i].
        i := i + 1].

Primes >> printPrimes
    primes do: [:each | Transcript show: each printString; cr].

"Exemplo de uso:"
| primeList |
primeList := Primes new.
primeList generatePrimes: 10.
primeList printPrimes.
```

Neste exemplo, criamos uma classe chamada `Primes` que é responsável por gerar e armazenar uma lista de números primos. 

Primeiramente, temos o método `initialize` que inicializa a instância da classe. Em seguida, temos o método `isPrime:`, que verifica se um número é primo ou não. Ele utiliza a propriedade `primes`, que é uma coleção ordenada, para verificar se algum número da lista de primos até a raiz quadrada do número em questão é um divisor. Se for, o método retorna `false`, caso contrário, retorna `true`.

O método `generatePrimes:` recebe como parâmetro a quantidade de números primos que se deseja gerar. Ele utiliza um loop while para verificar se a lista de primos possui menos elementos do que o count especificado. Caso a condição seja verdadeira, ele invoca o método `isPrime:` para verificar se o número atual é primo e o adiciona à lista caso seja.

Por fim, temos o método `printPrimes`, que imprime os números primos na saída do Transcript, uma janela de console no ambiente Smalltalk. Ele utiliza o método `do:` para percorrer a lista de primos e imprimir cada um deles na tela.

No exemplo de uso, criamos uma instância da classe `Primes` chamada `primeList`, geramos os 10 primeiros números primos e os imprimimos na tela.