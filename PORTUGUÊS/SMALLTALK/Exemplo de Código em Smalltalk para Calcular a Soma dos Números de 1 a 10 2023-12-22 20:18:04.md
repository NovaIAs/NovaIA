Claro! Aqui está um exemplo de código em Smalltalk:

```smalltalk
| listaNumeros soma |
listaNumeros := #(1 2 3 4 5 6 7 8 9 10).
soma := 0.

listaNumeros do: [:cadaNumero |
    soma := soma + cadaNumero.
].

Transcript show: 'A soma dos números de 1 a 10 é: ', soma printString.
```

Neste código, começamos criando uma variável chamada `listaNumeros` que contém os números de 1 a 10 em uma Array literal `#()`. Em seguida, criamos uma variável chamada `soma` e a inicializamos com o valor zero.

Utilizamos o método `do:` para percorrer cada elemento da lista de números. Dentro do bloco `[:cadaNumero | ... ]`, adicionamos cada número à variável `soma`. O operador `:=` é usado para atribuir o resultado da soma à variável `soma`.

Por fim, utilizamos o objeto `Transcript` para exibir a mensagem na saída do console. Usamos o método `show:` para mostrar a mensagem "A soma dos números de 1 a 10 é: " concatenada com o valor da variável `soma` convertido para String através do método `printString`.

Esse código em Smalltalk calcula e exibe a soma dos números de 1 a 10 usando um loop e a saída é exibida no console.