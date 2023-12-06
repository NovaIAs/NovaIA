Claro! Aqui está um código em Smalltalk, utilizando a linguagem natural da programação, em português:

```
"Objetivo: Verificar se um número é um quadrado perfeito."

"Definindo a classe QuadradoPerfeito"
Object subclass: #QuadradoPerfeito
    instanceVariableNames: ''

"Definindo o método de verificação"
QuadradoPerfeito>>verificarNumero: umNumero
    | raiz |
    raiz := umNumero sqrt.
    
    raiz isInteger ifTrue: [
        ^ 'O número ', umNumero printString, ' é um quadrado perfeito!' ].
    
    ^ 'O número ', umNumero printString, ' não é um quadrado perfeito!'

"Exemplo de uso"
| quadrado |
quadrado := QuadradoPerfeito new.
Transcript show: (quadrado verificarNumero: 16)."

```

Neste código, criamos a classe `QuadradoPerfeito` que possui um método `verificarNumero:`. 

O método `verificarNumero:` recebe um número como argumento e verifica se a raiz quadrada desse número é um número inteiro. 

Se a raiz for um número inteiro, o método retorna uma mensagem informando que o número é um quadrado perfeito. Caso contrário, retorna uma mensagem informando que o número não é um quadrado perfeito.

No exemplo de uso, criamos um objeto `quadrado` da classe `QuadradoPerfeito` e chamamos o método `verificarNumero:` passando o número 16 como argumento. O resultado é exibido no Transcript.