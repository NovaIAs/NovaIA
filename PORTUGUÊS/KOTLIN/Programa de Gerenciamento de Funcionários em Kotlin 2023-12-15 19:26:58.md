Certamente! Aqui está um exemplo de um código complexo em Kotlin, que usa a biblioteca padrão para criar um programa de gerenciamento de funcionários em uma empresa:

```kotlin
import java.util.Scanner

class Funcionario(val nome: String, val cargo: String, var salario: Double) {
    fun aumentarSalario(valor: Double) {
        salario += valor
    }
}

class Empresa(val nome: String, val cnpj: String, var funcionarios: MutableList<Funcionario> = mutableListOf()) {
    fun contratar(funcionario: Funcionario) {
        funcionarios.add(funcionario)
    }

    fun demitir(funcionario: Funcionario) {
        funcionarios.remove(funcionario)
    }

    fun aumentarSalarioTodos(valor: Double) {
        funcionarios.forEach { it.aumentarSalario(valor) }
    }

    fun imprimirFuncionarios() {
        println("Funcionários da empresa $nome:")
        funcionarios.forEach {
            println("Nome: ${it.nome}, Cargo: ${it.cargo}, Salário: R$ ${it.salario}")
        }
    }
}

fun main() {
    val scanner = Scanner(System.`in`)

    println("Bem-vindo ao programa de gerenciamento de funcionários!")

    println("Digite o nome da empresa:")
    val nomeEmpresa = scanner.nextLine()

    println("Digite o CNPJ da empresa:")
    val cnpjEmpresa = scanner.nextLine()

    val empresa = Empresa(nomeEmpresa, cnpjEmpresa)

    while (true) {
        println()
        println("Escolha uma opção:")
        println("1 - Contratar funcionário")
        println("2 - Demitir funcionário")
        println("3 - Aumentar salário de todos os funcionários")
        println("4 - Imprimir lista de funcionários")
        println("0 - Sair")

        val opcao = scanner.nextInt()

        when (opcao) {
            1 -> {
                println("Digite o nome do funcionário:")
                val nomeFuncionario = scanner.next()

                println("Digite o cargo do funcionário:")
                val cargoFuncionario = scanner.next()

                println("Digite o salário do funcionário:")
                val salarioFuncionario = scanner.nextDouble()

                val funcionario = Funcionario(nomeFuncionario, cargoFuncionario, salarioFuncionario)
                empresa.contratar(funcionario)

                println("Funcionário contratado com sucesso!")
            }
            2 -> {
                println("Digite o nome do funcionário a ser demitido:")
                val nomeFuncionario = scanner.next()

                val funcionario = empresa.funcionarios.find { it.nome == nomeFuncionario }

                if (funcionario != null) {
                    empresa.demitir(funcionario)
                    println("Funcionário demitido com sucesso!")
                } else {
                    println("Funcionário não encontrado.")
                }
            }
            3 -> {
                println("Digite o valor do aumento de salário:")
                val valorAumento = scanner.nextDouble()

                empresa.aumentarSalarioTodos(valorAumento)

                println("Salário de todos os funcionários aumentado com sucesso!")
            }
            4 -> {
                empresa.imprimirFuncionarios()
            }
            0 -> {
                println("Saindo do programa...")
                return
            }
            else -> {
                println("Opção inválida! Tente novamente.")
            }
        }
    }
}
```

Neste código, são definidas duas classes principais: `Funcionario` e `Empresa`. A classe `Funcionario` possui propriedades como nome, cargo e salário, além de um método para aumentar o salário. A classe `Empresa` possui propriedades como nome, CNPJ e uma lista de funcionários. Além disso, ela possui métodos para contratar/demitir funcionários, aumentar o salário de todos os funcionários e imprimir a lista de funcionários.

No `main()`, o programa recebe as informações da empresa (nome e CNPJ) e apresenta um menu com opções para o usuário. As opções incluem contratar/demitir funcionários, aumentar o salário de todos ou imprimir a lista de funcionários. A interação com o usuário é feita por meio da classe `Scanner`.

Espero que este código atenda aos seus requisitos!