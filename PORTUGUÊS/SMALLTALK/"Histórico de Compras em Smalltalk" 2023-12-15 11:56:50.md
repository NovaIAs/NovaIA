Claro! Aqui está um código em Smalltalk que cria um histórico de compras para um sistema de vendas online:

```
| historicoCompras |

historicoCompras := OrderedCollection new.

"Classe representando um item do carrinho de compras"
Object subclass: #ItemCarrinho
    instanceVariableNames: 'produto quantidade'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'SistemaDeVendasOnline'

ItemCarrinho>>initialize
    super initialize.
    produto := nil.
    quantidade := 0.

ItemCarrinho>>setProduto: aProduto quantidade: aQuantidade
    produto := aProduto.
    quantidade := aQuantidade.

ItemCarrinho>>produto
    ^ produto.

ItemCarrinho>>quantidade
    ^ quantidade.

"Classe representando um histórico de compras"
Object subclass: #HistoricoCompras
    instanceVariableNames: 'compras'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'SistemaDeVendasOnline'

HistoricoCompras>>initialize
    super initialize.
    compras := OrderedCollection new.

HistoricoCompras>>adicionarCompra: umItemCarrinho
    compras add: umItemCarrinho.

HistoricoCompras>>removerCompra: umItemCarrinho
    compras remove: umItemCarrinho ifAbsent: [].

HistoricoCompras>>totalCompras
    | total |
    total := 0.
    compras do: [ :item | total := total + (item quantidade) ].
    ^ total.

"Exemplo de uso"
| item1 item2 historico |
item1 := ItemCarrinho new.
item1 setProduto: 'Camiseta' quantidade: 2.

item2 := ItemCarrinho new.
item2 setProduto: 'Calça' quantidade: 1.

historico := HistoricoCompras new.
historico adicionarCompra: item1.
historico adicionarCompra: item2.

Transcript show: 'Total de compras: ', historico totalCompras asString.
```

Neste código, temos duas classes: `ItemCarrinho` e `HistoricoCompras`. A classe `ItemCarrinho` representa um item do carrinho de compras e possui duas variáveis de instância: `produto` e `quantidade`. Ela também possui métodos para configurar o produto e a quantidade, bem como para obter essas informações.

A classe `HistoricoCompras` representa o histórico de compras e tem uma variável de instância `compras`, que é uma coleção ordenada de itens do carrinho de compras. Ela possui métodos para adicionar e remover compras, bem como para calcular o total de compras feitas.

No exemplo de uso no final do código, criamos dois itens do carrinho de compras e os adicionamos ao histórico de compras. Então, exibimos o total de compras feitas.

Espero que este código atenda às suas expectativas. Lembre-se de que você pode personalizar e adaptar o código conforme suas necessidades.