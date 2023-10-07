Claro! Aqui está um exemplo de um código complexo em Kotlin:

```kotlin
// Classe modelo para representar uma Pessoa
data class Pessoa(val nome: String, val idade: Int, val altura: Double)

// Classe de serviço para realizar operações com as Pessoas
class ServicoPessoa(private val pessoas: MutableList<Pessoa>) {
    // Método para adicionar uma Pessoa à lista
    fun adicionarPessoa(pessoa: Pessoa) {
        pessoas.add(pessoa)
    }

    // Método para remover uma Pessoa da lista
    fun removerPessoa(pessoa: Pessoa) {
        pessoas.remove(pessoa)
    }

    // Método para buscar todas as Pessoas com idade maior que um valor específico
    fun buscarPorIdadeMaiorQue(valor: Int): List<Pessoa> {
        return pessoas.filter { it.idade > valor }
    }

    // Método para buscar todas as Pessoas com altura menor que um valor específico
    fun buscarPorAlturaMenorQue(valor: Double): List<Pessoa> {
        return pessoas.filter { it.altura < valor }
    }
}

fun main() {
    // Criação de algumas instâncias da classe Pessoa
    val pessoa1 = Pessoa("João", 25, 1.75)
    val pessoa2 = Pessoa("Maria", 30, 1.60)
    val pessoa3 = Pessoa("Pedro", 20, 1.80)

    // Criação de uma lista de Pessoas e adição das instâncias criadas anteriormente
    val listaPessoas = mutableListOf<Pessoa>()
    listaPessoas.add(pessoa1)
    listaPessoas.add(pessoa2)
    listaPessoas.add(pessoa3)

    // Criação de uma instância da classe ServicoPessoa, passando a lista de Pessoas como parâmetro
    val servicoPessoa = ServicoPessoa(listaPessoas)

    // Adição de uma nova Pessoa utilizando o método da classe ServicoPessoa
    servicoPessoa.adicionarPessoa(Pessoa("Ana", 28, 1.68))

    // Remoção de uma Pessoa utilizando o método da classe ServicoPessoa
    servicoPessoa.removerPessoa(pessoa2)

    // Busca de todas as Pessoas com idade maior que 25 utilizando o método da classe ServicoPessoa
    val pessoasComIdadeMaiorQue25 = servicoPessoa.buscarPorIdadeMaiorQue(25)
    println("Pessoas com idade maior que 25:")
    pessoasComIdadeMaiorQue25.forEach { println(it.nome) }

    // Busca de todas as Pessoas com altura menor que 1.70 utilizando o método da classe ServicoPessoa
    val pessoasComAlturaMenorQue170 = servicoPessoa.buscarPorAlturaMenorQue(1.70)
    println("Pessoas com altura menor que 1.70:")
    pessoasComAlturaMenorQue170.forEach { println(it.nome) }
}
```

Este código em Kotlin é um exemplo de um sistema simples de gerenciamento de pessoas. Ele consiste em três partes principais:

1. A classe `Pessoa` que define uma estrutura de dados para representar uma pessoa, com os atributos `nome`, `idade` e `altura`. Essa classe usa a palavra-chave `data` para gerar automaticamente métodos como `toString()`, `equals()`, `hashCode()`, entre outros.

2. A classe `ServicoPessoa`, que é responsável por realizar operações relacionadas às pessoas. Ela possui métodos para adicionar e remover pessoas da lista, além de métodos para buscar pessoas com base em critérios específicos, como idade maior que um determinado valor ou altura menor que um determinado valor.

3. A função `main()`, onde ocorre a execução do programa. Neste exemplo, são criadas algumas instâncias da classe `Pessoa`, adicionadas a uma lista e passadas como parâmetro para uma instância da classe `ServicoPessoa`. Em seguida, são realizadas operações como adição, remoção e busca de pessoas utilizando os métodos da classe `ServicoPessoa`. Os resultados são exibidos no console.

Espero que este exemplo tenha atendido às suas expectativas!