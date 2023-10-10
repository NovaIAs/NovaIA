Claro! Aqui está um exemplo de um código em Smalltalk que cria um jogo de adivinhação:

```
| numeroSecreto palpite tentativas |

"Define o número secreto"
numeroSecreto := Random new nextInt: 100.

"Define as variáveis iniciais"
palpite := 0.
tentativas := 0.

"Laço principal do jogo"
[palpite ~= numeroSecreto] whileTrue: [
    "Obtém o palpite do jogador"
    palpite := UIManager default request: 'Digite um número entre 1 e 100:'.

    "Incrementa o número de tentativas"
    tentativas := tentativas + 1.

    "Verifica se o palpite é correto"
    (palpite isNumber and: [palpite between: 1 and: 100]) ifTrue: [
        "Verifica se o palpite é maior, menor ou igual ao número secreto"
        (palpite > numeroSecreto) ifTrue: [
            Transcript show: 'Tente um número menor!'; cr.
        ] ifFalse: [
            (palpite < numeroSecreto) ifTrue: [
                Transcript show: 'Tente um número maior!'; cr.
            ] ifFalse: [
                Transcript show: 'Parabéns! Você acertou o número em ', tentativas asString, ' tentativas.'; cr.
            ].
        ].
    ] ifFalse: [
        Transcript show: 'Por favor, digite um número válido entre 1 e 100.'; cr.
    ].
].
```

Neste código, um número secreto é gerado aleatoriamente entre 1 e 100. O jogador deve fornecer um palpite e o jogo informará se o palpite é maior, menor ou igual ao número secreto. O jogo continuará até que o jogador acerte o número secreto.

O código utiliza um laço `whileTrue:` para repetir a solicitação de palpite até que o jogador acerte o número. Dentro do laço, o código verifica se o palpite é válido (um número entre 1 e 100) e então compara o palpite com o número secreto. Se o palpite for maior, uma mensagem é exibida indicando que o jogador deve tentar um número menor. Se o palpite for menor, uma mensagem é exibida indicando que o jogador deve tentar um número maior. Se o palpite for igual ao número secreto, uma mensagem de parabéns é exibida, junto com o número de tentativas realizadas.

Espero que este código atenda às suas expectativas!