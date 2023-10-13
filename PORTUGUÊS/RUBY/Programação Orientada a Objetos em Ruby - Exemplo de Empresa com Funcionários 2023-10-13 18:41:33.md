Claro! Aqui está um código complexo em Ruby que utiliza conceitos avançados de programação orientada a objetos e manipulação de dados:

```ruby
class Pessoa
  attr_accessor :nome, :idade

  def initialize(nome, idade)
    @nome = nome
    @idade = idade
  end

  def apresentar
    puts "Olá, meu nome é #{@nome} e tenho #{@idade} anos."
  end

  def calcular_aniversario(prox_aniversario)
    prox_aniversario ? @idade + 1 : @idade
  end
end

class Funcionario < Pessoa
  attr_accessor :cargo, :salario

  def initialize(nome, idade, cargo, salario)
    super(nome, idade)
    @cargo = cargo
    @salario = salario
  end

  def apresentar
    puts "Olá, meu nome é #{@nome}, tenho #{@idade} anos e sou #{@cargo}."
  end

  def calcular_aumento(percentual)
    @salario += @salario * (percentual / 100.0)
  end
end

class Empresa
  attr_accessor :nome, :funcionarios

  def initialize(nome)
    @nome = nome
    @funcionarios = []
  end

  def contratar(funcionario)
    @funcionarios << funcionario
  end

  def demitir(nome)
    @funcionarios.reject! { |funcionario| funcionario.nome == nome }
  end

  def listar_funcionarios
    puts "Funcionários da #{@nome}:"
    @funcionarios.each { |funcionario| puts funcionario.nome }
  end
end

empresa = Empresa.new("Exemplo")
funcionario1 = Funcionario.new("João", 25, "Programador", 5000)
funcionario2 = Funcionario.new("Maria", 30, "Analista", 6000)

empresa.contratar(funcionario1)
empresa.contratar(funcionario2)

empresa.listar_funcionarios

funcionario1.apresentar
funcionario2.apresentar

funcionario1.calcular_aumento(10)
puts "Novo salário do #{funcionario1.nome}: #{funcionario1.salario}"

idade_prox_aniversario = funcionario2.calcular_aniversario(true)
puts "Idade de #{funcionario2.nome} no próximo aniversário: #{idade_prox_aniversario}"
```

Neste código, temos três classes: `Pessoa`, `Funcionario` e `Empresa`. A classe `Pessoa` é a classe base, que representa uma pessoa genérica com nome e idade. A classe `Funcionario` herda de `Pessoa` e adiciona atributos específicos de um funcionário, como cargo e salário. A classe `Empresa` representa uma empresa e possui um nome e uma lista de funcionários.

No exemplo, criamos uma empresa chamada "Exemplo" e dois funcionários: João e Maria. Contratamos os funcionários na empresa e em seguida listamos todos os funcionários contratados. Cada funcionário é capaz de se apresentar, mostrando seu nome, idade e cargo. Além disso, é possível calcular o aumento salarial de um funcionário e calcular a idade no próximo aniversário.

Esse código demonstra a utilização de herança, polimorfismo e encapsulamento, conceitos essenciais em programação orientada a objetos.