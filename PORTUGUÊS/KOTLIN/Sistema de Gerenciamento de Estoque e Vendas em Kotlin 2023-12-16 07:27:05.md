Claro! Vou criar um código complexo em Kotlin para você. Este código será um sistema de gerenciamento de estoque de uma loja fictícia. Vou dividir o código em diferentes classes para facilitar a compreensão e a organização.

1. Classe Produto:
```kotlin
data class Produto(val id: Int, val nome: String, val preco: Double)
```
Esta classe representa um produto em nosso estoque. Cada produto possui um ID único, um nome e um preço.

2. Classe Estoque:
```kotlin
class Estoque {
    private val produtos: MutableList<Produto> = mutableListOf()

    fun adicionarProduto(produto: Produto) {
        produtos.add(produto)
    }

    fun removerProduto(id: Int) {
        val produto = produtos.find { it.id == id }
        produtos.remove(produto)
    }

    fun listarProdutos() {
        println("Produtos em estoque:")
        produtos.forEach { println("${it.id}: ${it.nome} - R$ ${it.preco}") }
    }
}
```
Esta classe representa o estoque da loja. Ela possui uma lista mutável de produtos e três métodos principais: `adicionarProduto`, `removerProduto` e `listarProdutos`.

3. Classe Venda:
```kotlin
data class Venda(val produto: Produto, val quantidade: Int)
```
Esta classe representa uma venda de um produto específico, juntamente com a quantidade vendida.

4. Classe Caixa:
```kotlin
class Caixa {
    private val vendas: MutableList<Venda> = mutableListOf()

    fun registrarVenda(venda: Venda) {
        vendas.add(venda)
    }

    fun calcularTotalVendas(): Double {
        var total = 0.0
        vendas.forEach { total += it.produto.preco * it.quantidade }
        return total
    }

    fun listarVendas() {
        println("Vendas realizadas:")
        vendas.forEach { println("- ${it.quantidade}x ${it.produto.nome} - R$ ${it.produto.preco * it.quantidade}") }
    }
}
```
Esta classe representa o caixa da loja, responsável por registrar as vendas e calcular o total de vendas realizadas.

5. Classe Principal:
```kotlin
fun main() {
    val estoque = Estoque()
    val caixa = Caixa()

    estoque.adicionarProduto(Produto(1, "Camiseta", 49.90))
    estoque.adicionarProduto(Produto(2, "Calça Jeans", 99.90))
    estoque.adicionarProduto(Produto(3, "Tênis", 199.90))

    estoque.listarProdutos()

    caixa.registrarVenda(Venda(estoque.listarProdutos()[0], 2))
    caixa.registrarVenda(Venda(estoque.listarProdutos()[1], 1))

    caixa.listarVendas()
    println("Total de vendas: R$ ${caixa.calcularTotalVendas()}")
}
```
Este é o código principal que utiliza as classes anteriores. Ele cria um estoque, adiciona alguns produtos, lista os produtos disponíveis, registra algumas vendas, lista as vendas realizadas e calcula o total de vendas.

Este código complexo em Kotlin representa um sistema de gerenciamento de estoque e vendas de uma loja fictícia. Ele utiliza classes, métodos e estruturas de controle para criar um sistema funcional e organizado.