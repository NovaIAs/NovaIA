Claro! Vou gerar um código complexo em Python para você. Este código é um exemplo de um sistema de gerenciamento de vendas para uma loja fictícia. Ele possui várias funcionalidades, como adicionar produtos ao estoque, registrar vendas, calcular lucro e gerar relatórios. Vamos lá:

```python
# Classe Produto para representar um produto
class Produto:
    def __init__(self, codigo, nome, preco):
        self.codigo = codigo
        self.nome = nome
        self.preco = preco

# Classe Estoque para gerenciar o estoque de produtos
class Estoque:
    def __init__(self):
        self.produtos = []

    def adicionar_produto(self, codigo, nome, preco):
        produto = Produto(codigo, nome, preco)
        self.produtos.append(produto)
        print(f"Produto '{nome}' adicionado ao estoque.")

    def buscar_produto(self, codigo):
        for produto in self.produtos:
            if produto.codigo == codigo:
                return produto
        return None

    def remover_produto(self, codigo):
        produto = self.buscar_produto(codigo)
        if produto:
            self.produtos.remove(produto)
            print(f"Produto '{produto.nome}' removido do estoque.")
        else:
            print("Produto não encontrado.")

    def listar_produtos(self):
        print("Produtos no estoque:")
        for produto in self.produtos:
            print(f"Código: {produto.codigo}, Nome: {produto.nome}, Preço: R${produto.preco}")

# Classe Venda para registrar as vendas
class Venda:
    def __init__(self, codigo_produto, quantidade):
        self.codigo_produto = codigo_produto
        self.quantidade = quantidade

# Classe GerenciadorVendas para gerenciar as vendas e calcular lucro
class GerenciadorVendas:
    def __init__(self):
        self.vendas = []
        self.lucro_total = 0

    def registrar_venda(self, codigo_produto, quantidade):
        venda = Venda(codigo_produto, quantidade)
        self.vendas.append(venda)
        print("Venda registrada.")

    def calcular_lucro(self, estoque):
        for venda in self.vendas:
            produto = estoque.buscar_produto(venda.codigo_produto)
            if produto:
                lucro_venda = produto.preco * venda.quantidade
                self.lucro_total += lucro_venda
        print(f"Lucro total: R${self.lucro_total}")

# Classe Relatorio para gerar relatórios
class Relatorio:
    def gerar_relatorio(self, estoque):
        print("Relatório de estoque:")
        estoque.listar_produtos()

# Função principal para interagir com o sistema
def main():
    estoque = Estoque()
    gerenciador_vendas = GerenciadorVendas()
    relatorio = Relatorio()

    while True:
        print("\n===== Sistema de Gerenciamento de Vendas =====")
        print("1 - Adicionar produto ao estoque")
        print("2 - Remover produto do estoque")
        print("3 - Registrar venda")
        print("4 - Gerar relatório de estoque")
        print("5 - Calcular lucro")
        print("0 - Sair")

        opcao = int(input("Escolha uma opção: "))

        if opcao == 1:
            codigo = int(input("Digite o código do produto: "))
            nome = input("Digite o nome do produto: ")
            preco = float(input("Digite o preço do produto: "))
            estoque.adicionar_produto(codigo, nome, preco)

        elif opcao == 2:
            codigo = int(input("Digite o código do produto: "))
            estoque.remover_produto(codigo)

        elif opcao == 3:
            codigo = int(input("Digite o código do produto: "))
            quantidade = int(input("Digite a quantidade vendida: "))
            gerenciador_vendas.registrar_venda(codigo, quantidade)

        elif opcao == 4:
            relatorio.gerar_relatorio(estoque)

        elif opcao == 5:
            gerenciador_vendas.calcular_lucro(estoque)

        elif opcao == 0:
            print("Saindo do sistema...")
            break

        else:
            print("Opção inválida.")

if __name__ == "__main__":
    main()
```

Este código é um exemplo completo de um sistema de gerenciamento de vendas em uma loja. Ele utiliza classes para representar produtos, estoque, vendas, gerenciador de vendas e relatórios. A função `main()` é responsável por interagir com o usuário por meio de um menu com várias opções.

O código permite adicionar produtos ao estoque, removê-los, registrar vendas, gerar relatórios de estoque e calcular o lucro total das vendas realizadas. Cada ação é executada de acordo com a opção escolhida pelo usuário no menu.