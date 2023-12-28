Claro! Aqui está um exemplo de código Kotlin complexo e diferenciado que utiliza recursos avançados da linguagem:

```
fun main() {
    val listaProdutos = mutableListOf<Produto>()

    // Criando objetos Produto
    val produto1 = Produto("Camiseta", 29.90)
    val produto2 = Produto("Calça Jeans", 99.90)
    val produto3 = Produto("Tênis", 149.90)

    // Adicionando os produtos à lista
    listaProdutos.add(produto1)
    listaProdutos.add(produto2)
    listaProdutos.add(produto3)

    // Ordenando a lista de produtos pelo preço de forma decrescente
    listaProdutos.sortByDescending { it.preco }

    // Imprimindo os produtos ordenados
    println("Produtos ordenados pelo preço:")
    for (produto in listaProdutos) {
        println("${produto.nome} - R$ %.2f".format(produto.preco))
    }
}

data class Produto(val nome: String, val preco: Double)
```

Neste exemplo, temos uma função `main()` que representa o ponto de entrada do programa. Dentro dela, criamos uma lista mutável de produtos chamada `listaProdutos`.

Em seguida, criamos três objetos da classe `Produto` com diferentes nomes e preços. Cada objeto é adicionado à lista de produtos usando o método `add()`.

Depois disso, utilizamos o método `sortByDescending()` para ordenar a lista de produtos pelo preço de forma decrescente, utilizando uma expressão lambda para especificar o critério de ordenação.

Por fim, percorremos a lista ordenada e imprimimos o nome e preço de cada produto utilizando a função `println()`.

A classe `Produto` é uma data class que possui dois atributos: `nome` e `preco`. Ela é utilizada para representar os produtos no programa.

Espero que este exemplo atenda às suas expectativas!