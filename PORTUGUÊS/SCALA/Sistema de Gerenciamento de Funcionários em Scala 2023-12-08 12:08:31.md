Claro! Aqui está um exemplo de código em Scala que cria um sistema de gerenciamento de funcionários em uma empresa:

```scala
import scala.collection.mutable.ArrayBuffer

// Classe Funcionário
class Funcionario(val nome: String, val idade: Int, val salario: Double) {
  override def toString: String = s"Nome: $nome, Idade: $idade, Salário: R$$salario"
}

// Classe que representa o Gerenciador de Funcionários
class GerenciadorFuncionarios {
  private val funcionarios = ArrayBuffer[Funcionario]()

  // Método para adicionar um novo funcionário
  def adicionarFuncionario(nome: String, idade: Int, salario: Double): Unit = {
    val funcionario = new Funcionario(nome, idade, salario)
    funcionarios += funcionario
  }

  // Método para remover um funcionário
  def removerFuncionario(nome: String): Unit = {
    val indice = funcionarios.indexWhere(_.nome == nome)
    if (indice != -1) funcionarios.remove(indice)
  }

  // Método para listar todos os funcionários
  def listarFuncionarios(): Unit = {
    if (funcionarios.isEmpty) {
      println("Não há funcionários cadastrados.")
    } else {
      println("Lista de Funcionários:")
      funcionarios.foreach(println)
    }
  }
}

// Criação do objeto GerenciadorFuncionarios
val gerenciador = new GerenciadorFuncionarios()

// Adicionando alguns funcionários
gerenciador.adicionarFuncionario("João", 25, 5000)
gerenciador.adicionarFuncionario("Maria", 30, 6000)
gerenciador.adicionarFuncionario("Pedro", 28, 5500)

// Removendo um funcionário
gerenciador.removerFuncionario("Maria")

// Listando todos os funcionários
gerenciador.listarFuncionarios()
```

Neste exemplo, temos duas classes em destaque: `Funcionario` e `GerenciadorFuncionarios`.

A classe `Funcionario` define as propriedades básicas de um funcionário, como nome, idade e salário. Essas propriedades são passadas no construtor da classe e são atribuídas a variáveis de instância.

A classe `GerenciadorFuncionarios` é responsável por gerenciar os funcionários. Ela possui um array mutável chamado `funcionarios` que irá armazenar todos os objetos `Funcionario`. 

O método `adicionarFuncionario` permite que um novo funcionário seja adicionado ao array `funcionarios`. Primeiro, é criado um novo objeto `Funcionario` com os parâmetros recebidos. Em seguida, o novo funcionário é adicionado ao array utilizando o operador `+=`.

O método `removerFuncionario` recebe o nome do funcionário que deve ser removido. Ele utiliza o método `indexWhere` para encontrar o índice do funcionário no array `funcionarios`. Se o índice for diferente de -1 (ou seja, o funcionário foi encontrado), o método `remove` é chamado para remover o funcionário do array.

O último método, `listarFuncionarios`, verifica se o array está vazio. Se estiver vazio, imprime uma mensagem informando que não há funcionários cadastrados. Caso contrário, imprime a lista de funcionários utilizando o método `foreach`.

Por fim, é criado um objeto `gerenciador` da classe `GerenciadorFuncionarios` e são realizadas algumas operações, como adicionar e remover funcionários, e listar todos os funcionários presentes no sistema.