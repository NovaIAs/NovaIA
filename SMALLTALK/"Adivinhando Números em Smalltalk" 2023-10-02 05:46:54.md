Claro! Aqui está um código em Smalltalk que cria um programa simples para adivinhar um número escolhido pelo usuário:

```smalltalk
| numeroPalpite numeroResposta tentativas |
tentativas := 0.
Transcript show: 'Bem-vindo ao jogo de adivinhação! Escolha um número entre 1 e 100: '.
numeroResposta := (Stdin nextLine asNumber) asInteger.
numeroPalpite := (1 to: 100) atRandom.
[tentativas < 10] whileTrue: [
    Transcript show: 'Eu acho que o número é: ', numeroPalpite printString; cr.
    tentativas := tentativas + 1.
    (numeroPalpite = numeroResposta) ifTrue: [
        Transcript show: 'Parabéns, você acertou o número!'; cr.
        ^ self.
    ] ifFalse: [
        Transcript show: 'Você errou! '.
        (numeroPalpite < numeroResposta) ifTrue: [
            Transcript show: 'O número que você está procurando é maior.'; cr.
        ] ifFalse: [
            Transcript show: 'O número que você está procurando é menor.'; cr.
        ].
        numeroPalpite := (1 to: 100) atRandom.
    ].
].
Transcript show: 'Você não conseguiu adivinhar o número. O número correto era: ', numeroResposta printString; cr.
```

Este código permite ao usuário escolher um número entre 1 e 100 e tenta adivinhá-lo em até 10 tentativas. O programa exibirá um palpite e, com base na resposta do usuário (maior, menor ou igual), ajustará o palpite na próxima iteração. Se o número for adivinhado corretamente, uma mensagem de parabéns será exibida. Caso contrário, uma mensagem informando o número correto será exibida.