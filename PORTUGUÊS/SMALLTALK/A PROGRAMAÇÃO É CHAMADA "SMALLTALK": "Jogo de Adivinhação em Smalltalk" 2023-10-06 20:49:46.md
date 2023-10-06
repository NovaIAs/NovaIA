Claro! Aqui está um exemplo de código em Smalltalk que cria um jogo de adivinhação:

```smalltalk
| numeroSecreto tentativa palpite |
Transcript show: 'Bem-vindo ao jogo de adivinhação!'.
Transcript show: 'Eu vou escolher um número entre 1 e 100 e você terá que adivinhar qual é!'.

numeroSecreto := (1 to: 100) atRandom.
tentativa := 0.

[ palpite := UIManager default request: 'Faça sua tentativa:'.
  tentativa := tentativa + 1.
  
  (palpite asNumber) = numeroSecreto ifTrue: [
    Transcript show: 'Parabéns! Você acertou em ' , tentativa asString , ' tentativas!'.
    ^self ].
  
  (palpite asNumber) < numeroSecreto ifTrue: [
    Transcript show: 'Seu palpite é menor que o número secreto.' ].
  
  (palpite asNumber) > numeroSecreto ifTrue: [
    Transcript show: 'Seu palpite é maior que o número secreto.' ].
  
  UIManager default inform: 'Você errou! Tente novamente.'.
] repeat.
```

Neste código, primeiro exibimos uma mensagem de boas-vindas e explicamos as regras do jogo. Em seguida, geramos um número secreto aleatório entre 1 e 100 usando o método `atRandom`. 

Dentro de um loop `repeat`, pedimos ao usuário para fazer uma tentativa de adivinhar o número secreto usando o método `request` do `UIManager`. A cada tentativa, incrementamos o contador de tentativas em 1.

Em seguida, comparamos o palpite do usuário com o número secreto. Se o palpite for igual ao número secreto, exibimos uma mensagem de parabéns com o número de tentativas e encerramos o jogo. Se o palpite for menor ou maior que o número secreto, exibimos uma mensagem indicando ao usuário se ele deve tentar um número maior ou menor.

Se o usuário errar o palpite, exibimos uma mensagem de erro usando o método `inform` do `UIManager` e o loop continua até que o usuário acerte o número secreto.

Este código cria uma experiência interativa para o usuário e demonstra como a linguagem Smalltalk pode ser usada para criar jogos simples.