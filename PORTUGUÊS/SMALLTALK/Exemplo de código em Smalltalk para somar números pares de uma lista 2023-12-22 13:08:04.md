Claro! Aqui está um exemplo de código em Smalltalk:

```smalltalk
| listaNumeros somaPares |

listaNumeros := #(1 2 3 4 5 6 7 8 9 10).
somaPares := 0.

listaNumeros do: [:numero |
    (numero \\ 2 = 0) ifTrue: [
        somaPares := somaPares + numero.
    ].
].

Transcript show: 'A soma dos números pares da lista é: ', somaPares asString.
```

Neste código em Smalltalk, começamos criando uma variável chamada `listaNumeros` que armazena uma lista de números de 1 a 10 usando a sintaxe `#()`.

Em seguida, criamos uma variável chamada `somaPares` e a inicializamos com zero.

Usamos o método `do:` para iterar sobre cada elemento da lista de números. O bloco de código dentro do `do:` é executado para cada número da lista.

Dentro do bloco, verificamos se o número é par usando a expressão `(numero \\ 2 = 0)`. O operador `\\` retorna o resto da divisão do número por 2. Se o resto for igual a zero, significa que o número é par.

Dentro do bloco condicional (`ifTrue:`), incrementamos a variável `somaPares` com o número atual.

Após o término do loop, usamos o método `asString` para converter a soma final em uma string e exibimos o resultado utilizando o `Transcript show:`.

O resultado será a soma dos números pares da lista impressa no Transcript.

Este código é um exemplo simples de como realizar a soma dos números pares em uma lista utilizando a linguagem Smalltalk.