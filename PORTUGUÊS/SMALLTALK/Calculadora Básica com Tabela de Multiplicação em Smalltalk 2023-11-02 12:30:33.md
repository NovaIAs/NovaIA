Claro! Aqui está um código em Smalltalk que implementa uma calculadora básica com uma funcionalidade adicional. Este código irá solicitar ao usuário que insira dois números e, em seguida, permitirá que o usuário escolha uma operação matemática para realizar. Além disso, o código também possui uma funcionalidade extra que exibe a tabela de multiplicação para qualquer número inserido.

```smalltalk
| numero1 numero2 operacao resultado |
Transcript show: 'Calculadora em Smalltalk'; cr.

numero1 := UIManager default request: 'Digite o primeiro número:'.
numero2 := UIManager default request: 'Digite o segundo número:'.

Transcript show: 'Operações disponíveis:'; cr.
Transcript show: '1. Adição'; cr.
Transcript show: '2. Subtração'; cr.
Transcript show: '3. Multiplicação'; cr.
Transcript show: '4. Divisão'; cr.
Transcript show: '5. Tabela de multiplicação'; cr.

operacao := UIManager default request: 'Escolha uma operação (1-5):'.

resultado := 0.

(operacao = '1') ifTrue: [
    resultado := numero1 + numero2.
    Transcript show: 'A soma de ', numero1 asString, ' e ', numero2 asString, ' é ', resultado asString.
] ifFalse: [(operacao = '2') ifTrue: [
    resultado := numero1 - numero2.
    Transcript show: 'A subtração de ', numero1 asString, ' e ', numero2 asString, ' é ', resultado asString.
] ifFalse: [(operacao = '3') ifTrue: [
    resultado := numero1 * numero2.
    Transcript show: 'A multiplicação de ', numero1 asString, ' e ', numero2 asString, ' é ', resultado asString.
] ifFalse: [(operacao = '4') ifTrue: [
    resultado := numero1 / numero2.
    Transcript show: 'A divisão de ', numero1 asString, ' por ', numero2 asString, ' é ', resultado asString.
] ifFalse: [(operacao = '5') ifTrue: [
    Transcript show: 'Tabela de multiplicação de ', numero1 asString, ':'; cr.
    1 to: 10 do: [:i |
        Transcript show: numero1 asString, ' x ', i asString, ' = ', (numero1 * i) asString; cr.
    ]
] ifFalse: [
    Transcript show: 'Operação inválida!'.
]]]].

Transcript show: 'Obrigado por utilizar a calculadora em Smalltalk!'; cr.
```

Este código começa solicitando ao usuário que insira dois números através do método `request` da classe `UIManager`. Em seguida, exibe as operações disponíveis na interface do usuário usando o método `show` da classe `Transcript`. O usuário é então solicitado a escolher uma operação, que é armazenada na variável `operacao`.

Dependendo da operação escolhida, o código realiza a operação matemática apropriada e exibe o resultado usando novamente o método `show` da classe `Transcript`. Se a operação escolhida for a opção 5, o código exibirá a tabela de multiplicação do primeiro número inserido.

No final, o código exibe uma mensagem de agradecimento ao usuário por usar a calculadora em Smalltalk.