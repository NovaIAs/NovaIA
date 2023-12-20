Claro! Aqui está um exemplo de código em Smalltalk que demonstra um jogo de adivinhação de números:

```smalltalk
| numeroAleatorio tentativa resultado |
numeroAleatorio := Random new nextInt: 100.
tentativa := 0.

Transcript cr; show: 'Bem-vindo ao jogo de adivinhação de números!'.
Transcript cr; show: 'Tente adivinhar o número de 0 a 100.'.

[tentativa <= 10] whileTrue: [
    | palpite |
    palpite := UIManager default request: 'Digite o seu palpite:'.
    
    (palpite isInteger and: [palpite between: 0 and: 100]) ifTrue: [
        tentativa := tentativa + 1.
        
        (palpite = numeroAleatorio) ifTrue: [
            Transcript cr; show: 'Parabéns! Você adivinhou o número em ', tentativa asString, ' tentativas.'.
            tentativa := 11.
        ] ifFalse: [
            resultado := palpite > numeroAleatorio
                ifTrue: ['Seu palpite é maior do que o número secreto.']
                ifFalse: ['Seu palpite é menor do que o número secreto.'].
            
            Transcript cr; show: resultado.
        ]
    ] ifFalse: [
        Transcript cr; show: 'Por favor, digite um número válido de 0 a 100.'
    ]
].

(tentativa > 10) ifTrue: [
    Transcript cr; show: 'Você excedeu o limite de tentativas. O número correto era ', numeroAleatorio asString, '.'.
].

Transcript cr; show: 'O jogo terminou. Obrigado por jogar!'.
```

Neste código, a variável `numeroAleatorio` armazena um número aleatório entre 0 e 100, gerado pela classe `Random`. A variável `tentativa` é utilizada para contar quantas tentativas o jogador fez.

O laço `whileTrue:` é usado para permitir que o jogador faça até 10 tentativas. Dentro do laço, é solicitado ao jogador que digite um palpite. Se o palpite for um número inteiro entre 0 e 100, o programa verifica se o palpite é igual ao número aleatório. Se for igual, o jogador acertou e o programa exibe a mensagem de parabéns. Caso contrário, o programa informa se o palpite é maior ou menor do que o número aleatório.

Se o palpite não for um número válido, é exibida uma mensagem de erro. Após 10 tentativas, o programa verifica se o jogador excedeu o limite e exibe o número correto.

Ao final do jogo, é exibida uma mensagem de encerramento.