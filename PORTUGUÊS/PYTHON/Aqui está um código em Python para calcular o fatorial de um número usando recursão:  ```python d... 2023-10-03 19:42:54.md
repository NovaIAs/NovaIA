Claro, vou gerar um código complexo em Python para você. Este código é um exemplo de um sistema de gerenciamento de estoque de uma loja. Ele permite adicionar produtos, remover produtos, atualizar informações e exibir relatórios. Vamos lá:

```python
class Produto:
    def __init__(self, codigo, nome, quantidade, preco):
        self.codigo = codigo
        self.nome = nome
        self.quantidade = quantidade
        self.preco = preco

class Estoque:
    def __init__(self):
        self.produtos = []

    def adicionar_produto(self, codigo, nome, quantidade, preco):
        produto = Produto(codigo, nome, quantidade, preco)
        self.produtos.append(produto)
        print("Produto adicionado com sucesso!")

    def remover_produto(self, codigo):
        for produto in self.produtos:
            if produto.codigo == codigo:
                self.produtos.remove(produto)
                print("Produto removido com sucesso!")
                return
        print("Produto não encontrado.")

    def atualizar_produto(self, codigo, nome, quantidade, preco):
        for produto in self.produtos:
            if produto.codigo == codigo:
                produto.nome = nome
                produto.quantidade = quantidade
                produto.preco = preco
                print("Produto atualizado com sucesso!")
                return
        print("Produto não encontrado.")

    def exibir_relatorio(self):
        print("Relatório de estoque:")
        for produto in self.produtos:
            print(f"Código: {produto.codigo}")
            print(f"Nome: {produto.nome}")
            print(f"Quantidade: {produto.quantidade}")
            print(f"Preço: R${produto.preco}")
            print("---------------------------")

# Exemplo de uso do sistema

estoque = Estoque()

estoque.adicionar_produto(1, "Camiseta", 10, 29.90)
estoque.adicionar_produto(2, "Calça", 5, 79.90)
estoque.adicionar_produto(3, "Tênis", 3, 149.90)

estoque.exibir_relatorio()

estoque.atualizar_produto(2, "Calça Jeans", 8, 89.90)

estoque.exibir_relatorio()

estoque.remover_produto(3)

estoque.exibir_relatorio()
```

Neste código, criamos duas classes: `Produto` e `Estoque`. A classe `Produto` representa um produto em si, com atributos como código, nome, quantidade e preço. Já a classe `Estoque` é responsável por gerenciar os produtos, permitindo adicionar, remover, atualizar e exibir relatórios.

No exemplo de uso do sistema, criamos uma instância da classe `Estoque` chamada `estoque`. Em seguida, adicionamos alguns produtos, exibimos o relatório, atualizamos um produto, exibimos o relatório novamente e, por fim, removemos um produto e exibimos o relatório mais uma vez.

Espero que este código atenda às suas necessidades!