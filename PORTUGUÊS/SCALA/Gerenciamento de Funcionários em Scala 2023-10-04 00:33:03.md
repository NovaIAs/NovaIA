Claro! Aqui está um código complexo em Scala que utiliza conceitos avançados da linguagem e design patterns. Ele é um sistema de gerenciamento de funcionários para uma empresa fictícia:

```scala
import scala.collection.mutable

// Definindo as classes de funcionários
sealed trait Funcionario {
  val id: Int
  val nome: String
  val salario: Double
}

case class Gerente(id: Int, nome: String, salario: Double) extends Funcionario
case class Desenvolvedor(id: Int, nome: String, salario: Double) extends Funcionario
case class Analista(id: Int, nome: String, salario: Double) extends Funcionario

// Classe responsável pela criação e gerenciamento dos funcionários
class Empresa {
  private val funcionarios: mutable.Map[Int, Funcionario] = mutable.Map.empty

  // Adiciona um novo funcionário à empresa
  def adicionarFuncionario(funcionario: Funcionario): Unit = {
    funcionarios += (funcionario.id -> funcionario)
    println(s"Funcionário ${funcionario.nome} adicionado com sucesso!")
  }

  // Remove um funcionário da empresa
  def removerFuncionario(id: Int): Unit = {
    funcionarios.remove(id) match {
      case Some(funcionario) => println(s"Funcionário ${funcionario.nome} removido com sucesso!")
      case None => println(s"Funcionário com ID $id não encontrado!")
    }
  }

  // Calcula o salário total da empresa
  def calcularSalarioTotal(): Double = {
    funcionarios.values.map(_.salario).sum
  }

  // Exibe a lista de funcionários da empresa
  def exibirFuncionarios(): Unit = {
    if (funcionarios.isEmpty) {
      println("Não há funcionários cadastrados.")
    } else {
      println("Funcionários da empresa:")
      funcionarios.values.foreach(funcionario => println(s"ID: ${funcionario.id}, Nome: ${funcionario.nome}, Salário: ${funcionario.salario}"))
    }
  }
}

// Exemplo de uso
object Main extends App {
  val empresa = new Empresa()

  val gerente = Gerente(1, "João", 5000)
  val desenvolvedor = Desenvolvedor(2, "Maria", 3000)
  val analista = Analista(3, "Pedro", 4000)

  empresa.adicionarFuncionario(gerente)
  empresa.adicionarFuncionario(desenvolvedor)
  empresa.adicionarFuncionario(analista)

  empresa.exibirFuncionarios()

  val salarioTotal = empresa.calcularSalarioTotal()
  println(s"Salário total da empresa: $salarioTotal")

  empresa.removerFuncionario(2)

  empresa.exibirFuncionarios()
}
```

Neste código, temos a definição das classes `Gerente`, `Desenvolvedor` e `Analista`, todas elas implementando a interface `Funcionario`, que representa um funcionário genérico com os atributos `id`, `nome` e `salario`. Em seguida, temos a classe `Empresa`, responsável por gerenciar os funcionários. Ela possui um mapa mutável de funcionários, onde a chave é o ID do funcionário e o valor é o objeto `Funcionario`.

A classe `Empresa` possui métodos para adicionar e remover funcionários, calcular o salário total da empresa e exibir a lista de funcionários. A adição e remoção de funcionários são feitas utilizando o ID como chave para o mapa. O método `calcularSalarioTotal()` utiliza a função `map` para extrair os salários de todos os funcionários e em seguida somá-los utilizando a função `sum`. O método `exibirFuncionarios()` verifica se a lista de funcionários está vazia e, caso não esteja, itera sobre eles utilizando `foreach` e os imprime no console.

No exemplo de uso no final do código, instanciamos a classe `Empresa`, criamos alguns funcionários (um `Gerente`, um `Desenvolvedor` e um `Analista`) e os adicionamos à empresa. Em seguida, exibimos a lista de funcionários, calculamos o salário total da empresa e removemos um funcionário. Por fim, exibimos novamente a lista de funcionários para verificar as alterações.

Esse código complexo em Scala demonstra o uso de classes, traits, coleções mutáveis, pattern matching e funções de alta ordem.