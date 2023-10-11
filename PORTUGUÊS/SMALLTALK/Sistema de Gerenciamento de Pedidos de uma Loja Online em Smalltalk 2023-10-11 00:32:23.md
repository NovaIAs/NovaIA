Claro, vou gerar um código em Smalltalk para você. Este código é um exemplo de um sistema de gerenciamento de pedidos de uma loja online. Ele inclui classes para representar produtos, clientes, pedidos e o carrinho de compras. Aqui está o código:

```
Object subclass: #Produto
    instanceVariableNames: 'codigo nome preco'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'LojaOnline'

Produto class>>newComCodigo: umCodigo nome: umNome preco: umPreco
    ^ self new
        codigo: umCodigo;
        nome: umNome;
        preco: umPreco;
        yourself

Produto>>codigo
    ^ codigo

Produto>>codigo: umCodigo
    codigo := umCodigo

Produto>>nome
    ^ nome

Produto>>nome: umNome
    nome := umNome

Produto>>preco
    ^ preco

Produto>>preco: umPreco
    preco := umPreco

Object subclass: #Cliente
    instanceVariableNames: 'nome endereco'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'LojaOnline'

Cliente class>>newComNome: umNome endereco: umEndereco
    ^ self new
        nome: umNome;
        endereco: umEndereco;
        yourself

Cliente>>nome
    ^ nome

Cliente>>nome: umNome
    nome := umNome

Cliente>>endereco
    ^ endereco

Cliente>>endereco: umEndereco
    endereco := umEndereco

Object subclass: #Pedido
    instanceVariableNames: 'cliente itens'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'LojaOnline'

Pedido class>>newComCliente: umCliente
    ^ self new
        cliente: umCliente;
        itens: OrderedCollection new;
        yourself

Pedido>>cliente
    ^ cliente

Pedido>>cliente: umCliente
    cliente := umCliente

Pedido>>itens
    ^ itens

Pedido>>adicionarItem: umItem
    itens add: umItem

Object subclass: #CarrinhoDeCompras
    instanceVariableNames: 'itens'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'LojaOnline'

CarrinhoDeCompras class>>new
    ^ self new
        itens: OrderedCollection new;
        yourself

CarrinhoDeCompras>>itens
    ^ itens

CarrinhoDeCompras>>itens: umaColecao
    itens := umaColecao

CarrinhoDeCompras>>adicionarItem: umItem
    itens add: umItem

CarrinhoDeCompras>>removerItem: umItem
    itens remove: umItem
```

Este código define quatro classes: `Produto`, `Cliente`, `Pedido` e `CarrinhoDeCompras`. A classe `Produto` representa um produto disponível na loja online, com atributos como código, nome e preço. A classe `Cliente` representa um cliente da loja, com atributos como nome e endereço. A classe `Pedido` representa um pedido feito por um cliente, com um cliente associado e uma coleção de itens. A classe `CarrinhoDeCompras` representa o carrinho de compras de um cliente, com uma coleção de itens.

Cada classe possui métodos para acessar e modificar os atributos relevantes. Por exemplo, a classe `Produto` possui métodos como `codigo`, `codigo:` para acessar e modificar o código do produto, `nome`, `nome:` para acessar e modificar o nome do produto, e assim por diante.

Esse código é apenas um exemplo básico de um sistema de gerenciamento de pedidos de uma loja online em Smalltalk. Você pode estender e modificar esse código conforme necessário para atender às suas necessidades.