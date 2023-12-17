Claro, vou gerar um código complexo em Python para você. Aqui está um exemplo de um sistema de gerenciamento de vendas em uma loja:

```python
import datetime

class Produto:
    def __init__(self, nome, preco):
        self.nome = nome
        self.preco = preco

class Venda:
    def __init__(self, produto, quantidade):
        self.produto = produto
        self.quantidade = quantidade
        self.data = datetime.datetime.now()

class Loja:
    def __init__(self):
        self.produtos = []
        self.vendas = []

    def adicionar_produto(self, produto):
        self.produtos.append(produto)
        print(f"O produto {produto.nome} foi adicionado à loja.")

    def remover_produto(self, produto):
        self.produtos.remove(produto)
        print(f"O produto {produto.nome} foi removido da loja.")

    def fazer_venda(self, produto, quantidade):
        if produto in self.produtos:
            if quantidade <= produto.quantidade:
                venda = Venda(produto, quantidade)
                self.vendas.append(venda)
                print(f"Venda realizada: {quantidade} unidades do produto {produto.nome}.")
                produto.quantidade -= quantidade
            else:
                print(f"Não há estoque suficiente para realizar a venda do produto {produto.nome}.")
        else:
            print(f"O produto {produto.nome} não está disponível na loja.")

    def listar_vendas(self):
        print("Vendas realizadas:")
        for venda in self.vendas:
            print(f"- Produto: {venda.produto.nome}, Quantidade: {venda.quantidade}, Data: {venda.data}")

    def listar_produtos(self):
        print("Produtos disponíveis:")
        for produto in self.produtos:
            print(f"- Nome: {produto.nome}, Preço: {produto.preco}, Quantidade: {produto.quantidade}")

loja = Loja()

produto1 = Produto("Camiseta", 29.90)
produto2 = Produto("Calça", 59.90)
produto3 = Produto("Tênis", 99.90)

loja.adicionar_produto(produto1)
loja.adicionar_produto(produto2)
loja.adicionar_produto(produto3)

loja.listar_produtos()

loja.fazer_venda(produto1, 3)
loja.fazer_venda(produto2, 2)
loja.fazer_venda(produto3, 1)

loja.listar_vendas()
loja.listar_produtos()

loja.remover_produto(produto2)

loja.listar_produtos()
```

Nesse código, temos uma classe `Produto` que representa cada produto da loja, com atributos como `nome` e `preco`. A classe `Venda` representa uma venda realizada, com atributos como o `produto`, a `quantidade` vendida e a `data` da venda.

A classe `Loja` é responsável por gerenciar os produtos e as vendas. Ela possui métodos para adicionar e remover produtos, fazer vendas, listar as vendas realizadas e listar os produtos disponíveis.

No exemplo, criamos uma instância da classe `Loja` chamada `loja` e alguns objetos da classe `Produto`. Em seguida, adicionamos os produtos à loja e listamos os produtos disponíveis. Depois, realizamos algumas vendas, listamos as vendas realizadas e os produtos disponíveis novamente. Por fim, removemos um produto da loja e listamos os produtos atualizados.