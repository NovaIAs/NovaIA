Certamente! Aqui está um exemplo de um código em Smalltalk que simula um jogo de adivinhação em que o computador pensa em um número e o jogador tenta adivinhar:

```
| numero numeroDoJogador tentativas continuar |

objetivoDesejado := Random new nextInt: 100.
tentativas := 0.
continuar := true.

Transcript cr; show: 'Bem-vindo ao jogo de adivinhação! O computador pensou em um número entre 1 e 100. Tente adivinhar qual é o número.'; cr.

[ continuar ] whileTrue: [
    tentativas := tentativas + 1.
    Transcript cr; show: 'Digite seu palpite: '.
    numeroDoJogador := Stdio readLine asNumber.
    
    (numeroDoJogador isNil) ifTrue: [
        Transcript cr; show: 'Entrada inválida. Por favor, digite um número válido entre 1 e 100.'; cr.
        continuar := true.
    ] ifFalse: [
        (numeroDoJogador = objetivoDesejado) ifTrue: [
            Transcript cr; show: 'Parabéns! Você acertou o número em ', tentativas printString, ' tentativas.'; cr.
            continuar := false.
        ] ifFalse: [
            Transcript cr; show: 'Número errado. Tente novamente.'; cr.
            continuar := true.
        ].
    ].
]
```

Neste código, o jogo começa gerando um número aleatório entre 1 e 100 usando a classe `Random`. Em seguida, o código solicita ao jogador que digite um palpite. O valor fornecido pelo jogador é armazenado na variável `numeroDoJogador`. 

O código verifica se o valor fornecido pelo jogador é válido, ou seja, se é um número válido. Se a entrada for inválida, o código exibe uma mensagem de erro e solicita ao jogador que faça uma nova tentativa. Caso contrário, o código verifica se o palpite do jogador corresponde ao número que o computador pensou. Se adivinhar corretamente, o código exibe uma mensagem de parabéns e informa a quantidade de tentativas feitas. Caso contrário, o código informa que o palpite está incorreto e continua pedindo ao jogador por um novo palpite.

O código continua executando até que o jogador adivinhe corretamente ou decida parar de jogar.

Note que este é apenas um exemplo de jogo de adivinhação em Smalltalk e pode ser personalizado de acordo com as necessidades do desenvolvedor.