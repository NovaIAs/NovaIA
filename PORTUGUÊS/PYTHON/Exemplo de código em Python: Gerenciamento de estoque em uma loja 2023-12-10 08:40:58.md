Claro! Aqui está um exemplo de código complexo em Python que utiliza funções, classes e estruturas de controle para criar um programa de gerenciamento de estoque em uma loja fictícia.

```python
class Produto:
    def __init__(self, codigo, nome, preco, quantidade):
        self.codigo = codigo
        self.nome = nome
        self.preco = preco
        self.quantidade = quantidade

class Loja:
    def __init__(self):
        self.estoque = []

    def adicionar_produto(self, produto):
        self.estoque.append(produto)
        print(f"Produto {produto.nome} adicionado ao estoque.")

    def remover_produto(self, codigo):
        for produto in self.estoque:
            if produto.codigo == codigo:
                self.estoque.remove(produto)
                print(f"Produto {produto.nome} removido do estoque.")
                return
        print("Produto não encontrado.")

    def exibir_estoque(self):
        print("Estoque atual:")
        for produto in self.estoque:
            print(f"Código: {produto.codigo}")
            print(f"Nome: {produto.nome}")
            print(f"Preço: R${produto.preco}")
            print(f"Quantidade: {produto.quantidade}")
            print("-----------------------")

# Criando instância da loja
loja = Loja()

# Adicionando produtos ao estoque
produto1 = Produto(1, "Camisa", 29.90, 10)
loja.adicionar_produto(produto1)

produto2 = Produto(2, "Calça", 59.90, 5)
loja.adicionar_produto(produto2)

# Exibindo estoque
loja.exibir_estoque()

# Removendo produto do estoque
loja.remover_produto(1)

# Exibindo estoque atualizado
loja.exibir_estoque()
```

Nesse código, temos duas classes: `Produto` e `Loja`. A classe `Produto` é responsável por representar cada item do estoque, enquanto a classe `Loja` é responsável por gerenciar o estoque como um todo.

A classe `Produto` possui um construtor `__init__` que recebe informações como código, nome, preço e quantidade. A classe `Loja` possui um atributo `estoque`, que é uma lista vazia inicialmente, e métodos como `adicionar_produto`, `remover_produto` e `exibir_estoque` para manipular o estoque.

No exemplo, são criados dois produtos e adicionados ao estoque da loja. Em seguida, o estoque é exibido. Depois, um produto é removido pelo seu código e o estoque é exibido novamente para mostrar a atualização.

Esse é apenas um exemplo de código complexo em Python. A complexidade pode variar dependendo dos requisitos do projeto e do contexto em que o código será aplicado.