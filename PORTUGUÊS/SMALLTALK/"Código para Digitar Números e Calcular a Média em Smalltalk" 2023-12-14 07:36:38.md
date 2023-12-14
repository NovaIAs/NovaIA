Claro, vou gerar um código em Smalltalk em português para você. Aqui está:

```
| listaNumeros somaMedia |

listaNumeros := OrderedCollection new.
somaMedia := 0.

[listaNumeros size < 10] whileTrue: [
    | numero |
    Transcript show: 'Digite um número: '.
    numero := stdin nextLine asNumber.
    listaNumeros add: numero.
    somaMedia := somaMedia + numero.
].

Transcript show: 'Números digitados: '.
listaNumeros do: [:cadaNumero | Transcript show: cadaNumero printString, ' '].

Transcript show: (String cr).
Transcript show: 'Soma total: ', somaMedia printString.
Transcript show: (String cr).
Transcript show: 'Média: ', (somaMedia / listaNumeros size) printString.
```

Neste código, criamos uma lista vazia chamada `listaNumeros` e uma variável `somaMedia` para armazenar a soma dos números digitados. Em seguida, utilizamos um loop `whileTrue` para continuar pedindo ao usuário para digitar números até que a lista tenha 10 elementos.

Dentro do loop, solicitamos que o usuário digite um número e o adicionamos à lista de números digitados. Em seguida, atualizamos a soma total `somaMedia` adicionando o número digitado.

Após sair do loop, exibimos todos os números digitados na ordem em que foram inseridos. Em seguida, exibimos a soma total dos números e a média calculada dividindo a soma pela quantidade de números na lista.

Este código é uma implementação simples para demonstrar a entrada de dados, manipulação de listas e cálculo da média em Smalltalk. Você pode adaptá-lo conforme suas necessidades.