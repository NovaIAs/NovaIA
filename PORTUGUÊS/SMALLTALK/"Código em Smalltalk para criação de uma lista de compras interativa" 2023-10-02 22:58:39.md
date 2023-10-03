Claro! Aqui está um exemplo de código em Smalltalk que cria uma lista de compras e permite ao usuário adicionar itens à lista:

| listaCompras |

listaCompras := OrderedCollection new.

[ | item |
Transcript cr; show: 'Digite um item para adicionar à lista de compras (ou digite "sair" para encerrar):'.
item := Transcript cr; readLine.
item = 'sair' ifTrue: [
Transcript cr; show: 'Lista de compras finalizada.'.
^listaCompras ].
listaCompras add: item.
Transcript cr; show: 'Item adicionado com sucesso!'.
Transcript cr; show: 'Itens na lista: ', listaCompras size printString.
Transcript cr; show: '-------------------------------------'.
] repeat.
```

Este código começa criando uma nova coleção ordenada chamada "listaCompras". Em seguida, entra em um loop infinito onde o usuário pode adicionar itens à lista de compras.

Dentro do loop, o código solicita ao usuário que digite um item para adicionar à lista. Se o usuário digitar "sair", o loop é encerrado e a lista de compras é exibida.

Se o usuário digitar qualquer outra coisa, o item é adicionado à lista com o método `add:` e uma mensagem de confirmação é exibida. O número de itens na lista também é exibido após cada adição.

O loop continua até que o usuário decida sair da lista de compras.