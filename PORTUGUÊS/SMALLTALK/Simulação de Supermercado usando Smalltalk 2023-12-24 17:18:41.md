Claro! Aqui está um exemplo de um código em Smalltalk que cria uma simulação de um supermercado:

```
"Definindo a classe Supermercado"
Object subclass: #Supermercado
	instanceVariableNames: 'estoque'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Supermercado'

"Definindo o método initialize"
Supermercado>>initialize
	super initialize.
	estoque := Dictionary new.

"Definindo o método adicionarProduto"
Supermercado>>adicionarProduto: nome quantidade: quantidade
	| produto |
	produto := estoque at: nome ifAbsent: [Produto new].
	produto adicionarQuantidade: quantidade.
	estoque at: nome put: produto.

"Definindo o método removerProduto"
Supermercado>>removerProduto: nome quantidade: quantidade
	| produto |
	produto := estoque at: nome ifAbsent: [^self].
	produto removerQuantidade: quantidade.
	estoque at: nome put: produto.

"Definindo a classe Produto"
Object subclass: #Produto
	instanceVariableNames: 'quantidade'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Supermercado'

"Definindo o método initialize"
Produto>>initialize
	super initialize.
	quantidade := 0.

"Definindo o método adicionarQuantidade"
Produto>>adicionarQuantidade: quantidade
	quantidade := quantidade + quantidade.

"Definindo o método removerQuantidade"
Produto>>removerQuantidade: quantidade
	quantidade := quantidade - quantidade.

"Exemplo de uso:"
| supermercado |
supermercado := Supermercado new.
supermercado adicionarProduto: 'Arroz' quantidade: 10.
supermercado adicionarProduto: 'Feijão' quantidade: 5.
supermercado adicionarProduto: 'Macarrão' quantidade: 8.
supermercado removerProduto: 'Arroz' quantidade: 3.
```

Neste exemplo, criamos duas classes: `Supermercado` e `Produto`. A classe `Supermercado` é responsável por gerenciar o estoque de produtos, enquanto a classe `Produto` representa um produto individual com sua quantidade.

No método `initialize` da classe `Supermercado`, inicializamos o estoque como um novo dicionário vazio. No método `adicionarProduto`, verificamos se o produto já existe no estoque. Se existir, apenas atualizamos a quantidade. Caso contrário, criamos um novo objeto `Produto` e adicionamos ao estoque. O método `removerProduto` faz a operação inversa.

Na classe `Produto`, o método `initialize` inicializa a quantidade como zero. O método `adicionarQuantidade` incrementa a quantidade atual com a quantidade passada como parâmetro, enquanto o método `removerQuantidade` faz a operação inversa.

No exemplo de uso, criamos uma instância de `Supermercado` chamada `supermercado`. Em seguida, adicionamos alguns produtos ao estoque usando o método `adicionarProduto`. Por fim, removemos uma quantidade específica de um produto usando o método `removerProduto`.