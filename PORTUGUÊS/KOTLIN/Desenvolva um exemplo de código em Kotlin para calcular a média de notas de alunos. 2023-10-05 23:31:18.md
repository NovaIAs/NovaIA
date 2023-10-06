Claro! Aqui está um exemplo de código complexo em Kotlin:

```kotlin
// Classe principal que representa um sistema de gerenciamento de vendas
class SistemaVendas {
    private val listaProdutos: MutableList<Produto> = mutableListOf() // Lista de produtos disponíveis
    private val listaClientes: MutableList<Cliente> = mutableListOf() // Lista de clientes cadastrados
    private val listaVendas: MutableList<Venda> = mutableListOf() // Lista de vendas realizadas

    // Método para adicionar um novo produto à lista de produtos
    fun adicionarProduto(produto: Produto) {
        listaProdutos.add(produto)
    }

    // Método para cadastrar um novo cliente
    fun cadastrarCliente(cliente: Cliente) {
        listaClientes.add(cliente)
    }

    // Método para realizar uma venda
    fun realizarVenda(cliente: Cliente, produtos: List<Produto>) {
        val venda = Venda(cliente, produtos)
        listaVendas.add(venda)
    }

    // Método para exibir o relatório de vendas
    fun exibirRelatorioVendas() {
        for (venda in listaVendas) {
            println("Cliente: ${venda.cliente.nome}")
            println("Produtos:")
            for (produto in venda.produtos) {
                println("- ${produto.nome}: R$ ${produto.preco}")
            }
            println("Total da venda: R$ ${venda.getTotal()}")
            println("---------------")
        }
    }
}

// Classe que representa um produto
class Produto(val nome: String, val preco: Double)

// Classe que representa um cliente
class Cliente(val nome: String)

// Classe que representa uma venda
class Venda(val cliente: Cliente, val produtos: List<Produto>) {
    // Método para calcular o total da venda
    fun getTotal(): Double {
        var total = 0.0
        for (produto in produtos) {
            total += produto.preco
        }
        return total
    }
}

fun main() {
    val sistemaVendas = SistemaVendas()

    val produto1 = Produto("Camiseta", 49.99)
    val produto2 = Produto("Calça", 99.99)
    val produto3 = Produto("Tênis", 199.99)

    sistemaVendas.adicionarProduto(produto1)
    sistemaVendas.adicionarProduto(produto2)
    sistemaVendas.adicionarProduto(produto3)

    val cliente1 = Cliente("João")
    val cliente2 = Cliente("Maria")

    sistemaVendas.cadastrarCliente(cliente1)
    sistemaVendas.cadastrarCliente(cliente2)

    val produtosVenda1 = listOf(produto1, produto2)
    val produtosVenda2 = listOf(produto3)

    sistemaVendas.realizarVenda(cliente1, produtosVenda1)
    sistemaVendas.realizarVenda(cliente2, produtosVenda2)

    sistemaVendas.exibirRelatorioVendas()
}
```

Neste exemplo, temos um sistema de gerenciamento de vendas implementado em Kotlin. A classe `SistemaVendas` é a classe principal que contém as listas de produtos, clientes e vendas. Ela possui métodos para adicionar produtos, cadastrar clientes e realizar vendas.

A classe `Produto` representa um produto e possui nome e preço. A classe `Cliente` representa um cliente e possui apenas o nome. A classe `Venda` representa uma venda e possui um cliente e uma lista de produtos. Ela também possui um método `getTotal()` que calcula o total da venda somando os preços dos produtos.

No método `main()`, são criados alguns produtos, clientes e realizadas algumas vendas de exemplo. Por fim, é exibido o relatório de vendas através do método `exibirRelatorioVendas()` do objeto `sistemaVendas`.

Espero que esse código atenda às suas expectativas!