Claro! Aqui está um exemplo de código em Smalltalk que cria um jogo de adivinhação em português:

```smalltalk
| numeroSecreto tentativa |
numeroSecreto := (1 to: 100) atRandom.
tentativa := 0.

[ tentativa = 0 ] whileTrue: [
    Transcript show: 'Bem-vindo ao jogo de adivinhação!' ; cr ;
                    show: 'Tente adivinhar um número entre 1 e 100:' ; cr.
    tentativa := Sensor readInteger ].

[ tentativa ~= numeroSecreto ] whileTrue: [
    tentativa > numeroSecreto ifTrue: [
        Transcript show: 'Você chutou alto! Tente um número menor.' ; cr.
    ] ifFalse: [
        Transcript show: 'Você chutou baixo! Tente um número maior.' ; cr.
    ].
    tentativa := Sensor readInteger
].

Transcript show: 'Parabéns! Você acertou o número secreto: ', numeroSecreto printString ; cr.
```

Neste código, utilizamos blocos de código (`[ ]`) para criar loops whileTrue e condicionais ifTrue. Primeiro, definimos a variável `numeroSecreto` com um número aleatório entre 1 e 100 usando o método `atRandom` da classe `Interval`. Também inicializamos a variável `tentativa` como 0.

Em seguida, usamos um loop whileTrue para exibir uma mensagem de boas-vindas e solicitar ao jogador que faça uma tentativa, usando o método `readInteger` da classe `Sensor` para ler a entrada do usuário.

Outro loop whileTrue é usado para comparar a tentativa do jogador com o número secreto. Se a tentativa for maior que o número secreto, uma mensagem é exibida indicando que o jogador chutou alto. Se a tentativa for menor, uma mensagem é exibida indicando que o jogador chutou baixo. O jogador é solicitado a fazer uma nova tentativa até que acerte o número secreto.

Quando o número secreto é finalmente adivinhado, uma mensagem de parabéns é exibida junto com o número secreto.

Espero que este exemplo tenha lhe dado uma ideia de como criar um código em Smalltalk para um jogo de adivinhação em português!