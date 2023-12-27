Claro! Aqui está um código em Smalltalk que cria um jogo de adivinhação em que o computador escolhe um número aleatório entre 1 e 100, e o jogador deve tentar adivinhar qual é esse número:

```smalltalk
| numeroAleatorio tentativa |
numeroAleatorio := (1 to: 100) atRandom. "Escolhe um número aleatório entre 1 e 100"

[ true ] whileTrue: [
    Transcript show: 'Digite um número entre 1 e 100: '. "Pede ao jogador para digitar um número"
    tentativa := (Stdin readLine asNumber ifNil: [ 0 ]). "Lê a tentativa do jogador e converte para número"
    
    tentativa = numeroAleatorio ifTrue: [ "Verifica se o jogador acertou"
        Transcript show: 'Parabéns, você acertou!'. "Exibe mensagem de acerto"
        ^ nil. "Finaliza o jogo"
    ] ifFalse: [
        tentativa < numeroAleatorio ifTrue: [ "Verifica se a tentativa é menor que o número aleatório"
            Transcript show: 'Tente um número maior!'. "Exibe mensagem de tentativa menor"
        ] ifFalse: [
            Transcript show: 'Tente um número menor!'. "Exibe mensagem de tentativa maior"
        ]
    ]
].
```

Explicação do código:

1. A variável `numeroAleatorio` é inicializada com um número aleatório entre 1 e 100, usando o método `atRandom` da classe `Interval`.

2. Em um loop infinito (`[ true ] whileTrue:`), o código pede ao jogador para digitar um número entre 1 e 100.

3. A entrada do jogador é lida usando o método `readLine` da classe `Stdin` e convertida para um número usando o método `asNumber`. Caso a conversão falhe, o valor padrão 0 é utilizado.

4. O código verifica se a tentativa do jogador é igual ao `numeroAleatorio`. Se for, exibe uma mensagem de parabéns e finaliza o jogo usando o operador `^` para sair do loop infinito.

5. Caso a tentativa seja menor que o `numeroAleatorio`, o código exibe uma mensagem pedindo para o jogador tentar um número maior.

6. Caso a tentativa seja maior que o `numeroAleatorio`, o código exibe uma mensagem pedindo para o jogador tentar um número menor.

O jogo continua até o jogador acertar o número ou decidir sair voluntariamente.