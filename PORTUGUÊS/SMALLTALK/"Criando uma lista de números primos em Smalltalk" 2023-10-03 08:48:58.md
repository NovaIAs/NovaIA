Claro! Aqui está um exemplo de código em Smalltalk que cria uma lista de números primos:

```smalltalk
Object subclass: #Primes
    instanceVariableNames: 'limit primes'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Primes'

Primes>>initialize
    super initialize.
    limit := 100.
    primes := OrderedCollection new.

Primes>>isPrime: n
    | i |
    n < 2 ifTrue: [^ false].
    primes do: [:p | (n rem: p) = 0 ifTrue: [^ false]].
    ^ true.

Primes>>generate
    | n |
    limit := limit + 100.
    n := primes isEmpty ifTrue: [2] ifFalse: [primes last + 1].
    [primes size < limit] whileTrue: [
        (self isPrime: n) ifTrue: [
            primes add: n.
            Transcript show: n asString; cr.
        ].
        n := n + 1
    ]
```

Explicando o código:

1. A classe `Primes` é criada como uma subclasse da classe `Object`.
2. Duas variáveis de instância são definidas: `limit` para controlar a quantidade de números primos a serem gerados e `primes` para armazenar os números primos.
3. O método `initialize` é definido para configurar os valores iniciais das variáveis de instância.
4. O método `isPrime:` recebe um número `n` como parâmetro e verifica se ele é primo.
5. O método `generate` gera a lista de números primos até o limite definido. Ele começa com o número 2 (o menor número primo) se a lista estiver vazia ou com o próximo número após o último primo da lista. Em seguida, ele itera até que a lista de primos atinja o limite desejado, verificando se cada número é primo com base no método `isPrime:` e adicionando-o à lista de primos se for verdadeiro.
6. Durante a geração, o número primo é exibido no console usando o método `Transcript show:`.

Esse código em Smalltalk cria uma classe `Primes` que pode ser instanciada para gerar uma lista de números primos. A lista é gerada incrementalmente, adicionando um número por vez até atingir o limite definido. Cada número é verificado se é primo e, se for, é adicionado à lista.