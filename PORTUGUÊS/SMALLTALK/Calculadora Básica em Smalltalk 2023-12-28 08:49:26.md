Claro! Aqui está um exemplo de código em Smalltalk que implementa uma calculadora básica:

```
| Calculadora |
Calculadora := Object new.

Calculadora soma: [ :a :b | a + b ].
Calculadora subtrai: [ :a :b | a - b ].
Calculadora multiplica: [ :a :b | a * b ].
Calculadora divide: [ :a :b |
    b = 0
        ifTrue: [ Transcript show: 'Não é possível dividir por zero.'; cr ]
        ifFalse: [ a / b ] ].

Transcript show: 'Bem-vindo à calculadora em Smalltalk!' ; cr.
Transcript show: 'Digite a operação desejada: soma, subtrai, multiplica ou divide.' ; cr.
| operacao a b resultado |
operacao := Transcript cr; nextChunk.
a := Transcript cr; nextChunk asNumber.
b := Transcript cr; nextChunk asNumber.

resultado := Calculadora perform: operacao withArguments: { a . b }.

Transcript show: 'O resultado da operação ' , operacao , ' entre ' , a printString , ' e ' , b printString , ' é: ' , resultado printString ; cr.
```

Este código cria uma classe de calculadora em Smalltalk, que permite ao usuário realizar operações de soma, subtração, multiplicação e divisão. 

O código cria uma instância da classe Calculadora e define os métodos de soma, subtração, multiplicação e divisão. 

No método de divisão, é verificado se o divisor é zero para evitar erros de divisão por zero. Caso seja, uma mensagem de erro é exibida no Transcript.

Em seguida, o código solicita ao usuário que digite a operação desejada e os números para realizar a operação. O resultado é calculado chamando o método correspondente na instância da calculadora.

Por fim, o resultado é exibido no Transcript.

Espero que isso tenha ajudado!