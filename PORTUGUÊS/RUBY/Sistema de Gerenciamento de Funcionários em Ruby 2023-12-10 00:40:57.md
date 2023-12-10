Claro! Aqui está um código em Ruby que implementa um sistema de gerenciamento de funcionários de uma empresa:

```ruby
class Funcionario
  attr_accessor :nome, :idade, :cargo, :salario

  def initialize(nome, idade, cargo, salario)
    @nome = nome
    @idade = idade
    @cargo = cargo
    @salario = salario
  end

  def imprimir_informacoes
    puts "Nome: #{@nome}"
    puts "Idade: #{@idade}"
    puts "Cargo: #{@cargo}"
    puts "Salário: #{@salario}"
  end
end

class Empresa
  attr_accessor :funcionarios

  def initialize
    @funcionarios = []
  end

  def adicionar_funcionario(funcionario)
    @funcionarios << funcionario
  end

  def imprimir_funcionarios
    puts "Funcionários da empresa:"
    @funcionarios.each do |funcionario|
      funcionario.imprimir_informacoes
      puts "----"
    end
  end

  def aumentar_salarios(percentual)
    @funcionarios.each do |funcionario|
      funcionario.salario *= (1 + percentual.to_f/100)
    end
  end
end

# Criação de objetos Funcionario
funcionario1 = Funcionario.new("João", 30, "Desenvolvedor", 5000)
funcionario2 = Funcionario.new("Maria", 25, "Analista", 4000)
funcionario3 = Funcionario.new("Carlos", 35, "Gerente", 8000)

# Criação de objeto Empresa
empresa = Empresa.new

# Adicionar funcionários à empresa
empresa.adicionar_funcionario(funcionario1)
empresa.adicionar_funcionario(funcionario2)
empresa.adicionar_funcionario(funcionario3)

# Imprimir informações dos funcionários
empresa.imprimir_funcionarios

# Aumentar salários em 10%
empresa.aumentar_salarios(10)

# Imprimir informações atualizadas dos funcionários
empresa.imprimir_funcionarios
```

Neste código, temos duas classes principais: `Funcionario` e `Empresa`. A classe `Funcionario` possui atributos como nome, idade, cargo e salário, além de um método `imprimir_informacoes` que mostra todas essas informações na tela.

A classe `Empresa` possui um atributo `funcionarios`, que é uma lista de funcionários, e métodos para adicionar funcionários, imprimir os funcionários cadastrados e aumentar os salários de todos os funcionários em um determinado percentual.

No final do código, criamos alguns objetos da classe `Funcionario`, adicionamos esses funcionários à empresa, imprimimos as informações dos funcionários, aumentamos os salários em 10% e, por fim, imprimimos as informações atualizadas dos funcionários.

Esse código mostra um exemplo simples de como utilizar classes e objetos em Ruby para criar um sistema de gerenciamento de funcionários. Claro, você pode adaptar e adicionar mais funcionalidades conforme suas necessidades.