Claro! Aqui está um código em Smalltalk que cria um jogo de adivinhação em que o computador tenta adivinhar um número pensado pelo jogador:

```smalltalk
| numeroPensado palpite min max resposta |

numeroPensado := 0.
palpite := 0.
min := 1.
max := 100.
resposta := ''.

Transcript cr; show: 'Pense em um número entre 1 e 100.'; cr.

[numeroPensado = 0] whileTrue: [
    Transcript show: 'Digite o número que pensou: '.
    numeroPensado := Smalltalk readFromString: (Smalltalk stdin nextLine).
].

[ resposta ~= 'acertou' ] whileTrue: [
    palpite := min + (max - min + 1) // 2.
    
    Transcript cr; show: 'O número é '; show: palpite asString; cr.
    
    Transcript cr; show: 'Digite "maior" se o número pensado for maior, "menor" se for menor, ou "acertou" se eu adivinhei corretamente: '.
    resposta := Smalltalk readFromString: (Smalltalk stdin nextLine).
    
    resposta = 'maior' ifTrue: [ min := palpite + 1 ].
    resposta = 'menor' ifTrue: [ max := palpite - 1 ].
].

Transcript cr; show: 'Eu acertei! O número pensado era: '; show: palpite asString; cr.
```

Neste código em Smalltalk, primeiro pedimos ao jogador para pensar em um número entre 1 e 100. Em seguida, o número pensado é armazenado na variável `numeroPensado`.

Em um loop `whileTrue`, o computador faz um palpite inicial utilizando o método de busca binária. Ele pega a média do menor valor possível (`min`) e o maior valor possível (`max`) e, em seguida, faz uma adivinhação.

Após cada palpite, o computador pede ao jogador para informar se o número pensado é maior, menor ou se o palpite foi correto. A resposta do jogador é armazenada na variável `resposta`.

O loop continua enquanto a resposta não for "acertou". Se a resposta informada for "maior", atualizamos o valor mínimo (`min`) para o palpite atual mais 1. Se a resposta for "menor", atualizamos o valor máximo (`max`) para o palpite atual menos 1.

Quando o jogador informar "acertou", o loop é encerrado e o computador exibe a mensagem "Eu acertei! O número pensado era: " seguido do palpite final.

Este código é um exemplo simples de um jogo de adivinhação em Smalltalk, onde a inteligência artificial tenta adivinhar o número pensado pelo jogador utilizando o método de busca binária.