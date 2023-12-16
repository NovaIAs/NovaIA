Claro! Segue abaixo um código complexo em Scala, que implementa um sistema de gerenciamento de funcionários de uma empresa fictícia:

```scala
import scala.collection.mutable.ListBuffer

case class Funcionario(nome: String, cargo: String, salario: Double)

class Empresa(var funcionarios: ListBuffer[Funcionario]) {

  def contratar(funcionario: Funcionario): Unit = {
    funcionarios += funcionario
    println(s"O funcionário ${funcionario.nome} foi contratado com sucesso!")
  }

  def demitir(nome: String): Unit = {
    val funcionarioOpt = funcionarios.find(_.nome == nome)
    funcionarioOpt match {
      case Some(funcionario) =>
        funcionarios -= funcionario
        println(s"O funcionário ${funcionario.nome} foi demitido com sucesso!")
      case None =>
        println(s"Não foi encontrado nenhum funcionário com o nome $nome.")
    }
  }

  def aumentarSalario(nome: String, aumento: Double): Unit = {
    val funcionarioOpt = funcionarios.find(_.nome == nome)
    funcionarioOpt match {
      case Some(funcionario) =>
        val salarioAntigo = funcionario.salario
        val novoSalario = salarioAntigo + aumento
        funcionario.salario = novoSalario
        println(s"O funcionário ${funcionario.nome} teve seu salário aumentado de $salarioAntigo para $novoSalario.")
      case None =>
        println(s"Não foi encontrado nenhum funcionário com o nome $nome.")
    }
  }

  def listarFuncionarios(): Unit = {
    println("Lista de funcionários:")
    funcionarios.foreach(funcionario => println(s"Nome: ${funcionario.nome}, Cargo: ${funcionario.cargo}, Salário: ${funcionario.salario}"))
  }

}

object Main extends App {

  val empresa = new Empresa(ListBuffer())

  // Contratando funcionários
  empresa.contratar(Funcionario("Maria", "Gerente", 5000.0))
  empresa.contratar(Funcionario("João", "Desenvolvedor", 3000.0))
  empresa.contratar(Funcionario("Pedro", "Analista", 4000.0))

  // Listando funcionários
  empresa.listarFuncionarios()

  // Aumentando salário do funcionário João
  empresa.aumentarSalario("João", 1000.0)

  // Demitindo funcionário Pedro
  empresa.demitir("Pedro")

  // Listando funcionários após as alterações
  empresa.listarFuncionarios()

}
```

Neste código, temos a definição da classe `Funcionario`, que possui os atributos `nome`, `cargo` e `salario`. Em seguida, temos a classe `Empresa`, que possui uma lista mutável de funcionários e métodos para contratar, demitir, aumentar o salário e listar os funcionários.

Na classe `Empresa`, o método `contratar` adiciona um funcionário à lista `funcionarios` e exibe uma mensagem de sucesso. O método `demitir` remove um funcionário da lista pelo nome e exibe uma mensagem indicando se a demissão foi realizada com sucesso ou se nenhum funcionário foi encontrado com o nome informado.

O método `aumentarSalario` aumenta o salário de um funcionário pelo nome e exibe uma mensagem informando o novo valor do salário. Caso nenhum funcionário seja encontrado com o nome informado, uma mensagem de erro é exibida.

O método `listarFuncionarios` percorre a lista de funcionários e exibe suas informações na saída.

No objeto `Main`, são criados alguns funcionários, utilizando o método `contratar`, e em seguida são realizadas algumas operações, como aumento de salário e demissão, utilizando os métodos da classe `Empresa`. Por fim, a lista de funcionários é exibida antes e depois das alterações.