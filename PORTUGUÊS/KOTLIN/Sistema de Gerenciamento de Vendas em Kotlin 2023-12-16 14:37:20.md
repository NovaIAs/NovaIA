Claro! Aqui está um código Kotlin complexo e diferenciado, que demonstra um exemplo de um sistema de gerenciamento de vendas para uma loja virtual:

```kotlin
import java.text.SimpleDateFormat
import java.util.*

data class Produto(val nome: String, val preco: Double)

data class Cliente(val nome: String, val email: String)

data class Pedido(val numero: Int, val data: Date, val cliente: Cliente, val produtos: List<Produto>)

class LojaVirtual {
    private var pedidos: MutableList<Pedido> = mutableListOf()
    private var numeroPedido = 1

    fun realizarPedido(cliente: Cliente, produtos: List<Produto>) {
        val dataAtual = Date()
        val pedido = Pedido(numeroPedido, dataAtual, cliente, produtos)
        pedidos.add(pedido)
        numeroPedido++

        exibirDetalhesPedido(pedido)
        enviarEmailConfirmacao(cliente, pedido)
    }

    private fun exibirDetalhesPedido(pedido: Pedido) {
        val formatadorData = SimpleDateFormat("dd/MM/yyyy HH:mm:ss", Locale.getDefault())
        val dataFormatada = formatadorData.format(pedido.data)

        println("Pedido Número: ${pedido.numero}")
        println("Data: $dataFormatada")
        println("Cliente: ${pedido.cliente.nome}")
        println("Email: ${pedido.cliente.email}")
        println("Produtos:")
        pedido.produtos.forEach { produto ->
            println(" - ${produto.nome}: R$ ${produto.preco}")
        }
        println("Total: R$ ${calcularTotalPedido(pedido.produtos)}")
    }

    private fun calcularTotalPedido(produtos: List<Produto>): Double {
        var total = 0.0
        produtos.forEach { produto ->
            total += produto.preco
        }
        return total
    }

    private fun enviarEmailConfirmacao(cliente: Cliente, pedido: Pedido) {
        println("Enviando email de confirmação para ${cliente.email}")
        // Lógica para enviar o email de confirmação
        println("Email de confirmação enviado com sucesso!")
    }
}

fun main() {
    val lojaVirtual = LojaVirtual()

    val cliente1 = Cliente("João Silva", "joao.silva@email.com")
    val produtos1 = listOf(
        Produto("Camiseta", 39.90),
        Produto("Calça Jeans", 99.90),
        Produto("Tênis", 149.90)
    )
    lojaVirtual.realizarPedido(cliente1, produtos1)

    val cliente2 = Cliente("Maria Santos", "maria.santos@email.com")
    val produtos2 = listOf(
        Produto("Vestido", 79.90),
        Produto("Sapato", 89.90),
        Produto("Bolsa", 59.90)
    )
    lojaVirtual.realizarPedido(cliente2, produtos2)
}
```

Neste código, criamos as classes `Produto`, `Cliente` e `Pedido` para representar as entidades envolvidas no sistema de gerenciamento de vendas. A classe `LojaVirtual` é responsável por gerenciar os pedidos e realizar as operações relacionadas a eles.

No método `realizarPedido`, recebemos o cliente e uma lista de produtos selecionados pelo cliente. Em seguida, geramos um número de pedido único, com base em uma variável de controle `numeroPedido`. Criamos um objeto `Pedido` com as informações do cliente, data e produtos, e adicionamos esse pedido à lista de pedidos da loja.

Após isso, chamamos os métodos `exibirDetalhesPedido` e `enviarEmailConfirmacao`. O primeiro exibe na tela os detalhes do pedido, como número, data, cliente, produtos e total. O segundo método simula o envio de um email de confirmação para o cliente.

No método `calcularTotalPedido`, percorremos a lista de produtos e somamos os preços de cada um para obter o total do pedido.

Por fim, no método `main`, criamos uma instância da classe `LojaVirtual` e realizamos dois pedidos de exemplo, com diferentes clientes e produtos.

Espero que este código atenda às suas expectativas!