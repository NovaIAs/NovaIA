Claro! Aqui está um exemplo de código em Smalltalk que cria uma lista de compras interativa:

```smalltalk
| listaCompras itensComprados |
listaCompras := OrderedCollection new.

[ | item |
Transcript cr; show: 'Digite o item que deseja adicionar à lista de compras (ou digite "sair" para encerrar):'.
item := Transcript cr; nextChunk.
item = 'sair' ifTrue: [ ^listaCompras ].
listaCompras add: item.
] repeat.

Transcript cr; show: 'Lista de compras:'.
listaCompras withIndexDo: [:item :index |
Transcript cr; show: index asString, '. ', item.
].

Transcript cr; show: 'Digite o número do item que deseja marcar como comprado (ou digite "sair" para encerrar):'.
itensComprados := OrderedCollection new.

[ | item |
Transcript cr; show: 'Digite o número do item que deseja marcar como comprado (ou digite "sair" para encerrar):'.
item := Transcript cr; nextChunk.
item = 'sair' ifTrue: [ ^itensComprados ].
item asNumber ifNil: [
Transcript cr; show: 'Entrada inválida! Por favor, digite apenas números.'; cr.
] ifNotNil: [
(item asNumber between: 1 and: listaCompras size) ifTrue: [
itensComprados add: (listaCompras at: item asNumber).
listaCompras removeAt: item asNumber.
] ifFalse: [
Transcript cr; show: 'Número inválido! Por favor, digite um número válido.'; cr.
].
].
] repeat.

Transcript cr; show: 'Itens comprados:'.
itensComprados withIndexDo: [:item :index |
Transcript cr; show: index asString, '. ', item.
].

Transcript cr; show: 'Itens restantes na lista de compras:'.
listaCompras withIndexDo: [:item :index |
Transcript cr; show: index asString, '. ', item.
].
```

Neste código, o programa solicita ao usuário que insira os itens desejados na lista de compras. Os itens são adicionados a uma coleção ordenada chamada `listaCompras`. O usuário pode digitar "sair" para encerrar a entrada de itens.

Em seguida, o programa exibe a lista de compras para o usuário. O usuário então pode digitar o número do item que deseja marcar como comprado. Os itens comprados são removidos da lista de compras e adicionados a uma nova coleção chamada `itensComprados`.

Por fim, o programa exibe os itens comprados e os itens restantes na lista de compras.

Espero que isso atenda às suas necessidades! Se você tiver alguma dúvida, estou à disposição para ajudar.