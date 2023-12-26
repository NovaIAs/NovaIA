Claro! Aqui está um exemplo de código em Smalltalk que cria um jogo de adivinhação de números:

```smalltalk
| numeroAleatorio palpite tentativas |
tentativas := 0.

"Gerar um número aleatório de 1 a 100"
numeroAleatorio := 1 + (100 atRandom).

"Executar o loop principal do jogo"
[ palpite := UIManager default request: 'Digite um número de 1 a 100:'.
  palpite isEmptyOrNil ifTrue: [ ^'Jogo encerrado!' ].
  
  palpite := palpite asNumber.
  tentativas := tentativas + 1.
  
  palpite = numeroAleatorio ifTrue: [
    ^'Parabéns! Você acertou o número em ', tentativas printString, ' tentativas!'
  ].
  
  palpite > numeroAleatorio ifTrue: [
    UIManager default inform: 'Seu palpite é muito alto. Tente novamente.'.
  ] ifFalse: [
    UIManager default inform: 'Seu palpite é muito baixo. Tente novamente.'.
  ]
] repeat.
```

Este código em Smalltalk cria um jogo de adivinhação de números. Ele gera um número aleatório entre 1 e 100 e, em seguida, solicita ao usuário que digite um número. O programa verifica se o palpite está correto, muito alto ou muito baixo e exibe as mensagens adequadas. O jogo continua até que o usuário acerte o número ou decida encerrar o jogo. Ao final, o programa exibe a quantidade de tentativas que o usuário fez para acertar o número.

Espero que esse código lhe seja útil!