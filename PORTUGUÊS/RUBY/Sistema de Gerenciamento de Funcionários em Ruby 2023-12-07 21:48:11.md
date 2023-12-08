Claro! Aqui está um exemplo de um código complexo em Ruby, que utiliza classes, métodos e herança para criar um sistema de gerenciamento de funcionários:

```ruby
class Funcionario
  attr_accessor :nome, :idade, :salario

  def initialize(nome, idade, salario)
    @nome = nome
    @idade = idade
    @salario = salario
  end

  def exibir_informacoes
    puts "Nome: #{@nome}"
    puts "Idade: #{@idade}"
    puts "Salário: #{@salario}"
  end
end

class Programador < Funcionario
  attr_accessor :linguagem_principal

  def initialize(nome, idade, salario, linguagem_principal)
    super(nome, idade, salario)
    @linguagem_principal = linguagem_principal
  end

  def exibir_informacoes
    super
    puts "Linguagem Principal: #{@linguagem_principal}"
  end

  def aumentar_salario(percentual)
    @salario += (@salario * percentual / 100)
  end
end

class Gerente < Funcionario
  attr_accessor :equipe

  def initialize(nome, idade, salario, equipe)
    super(nome, idade, salario)
    @equipe = equipe
  end

  def exibir_informacoes
    super
    puts "Equipe: #{@equipe}"
  end

  def aumentar_salario(percentual)
    @salario += (@salario * percentual / 100)
  end
end

# Exemplo de uso:
programador = Programador.new("João", 25, 5000, "Ruby")
gerente = Gerente.new("Maria", 35, 10000, "Equipe de Desenvolvimento")

programador.exibir_informacoes
puts "=========="
gerente.exibir_informacoes
puts "=========="
programador.aumentar_salario(10)
gerente.aumentar_salario(15)
programador.exibir_informacoes
puts "=========="
gerente.exibir_informacoes
```

Nesse código, temos três classes: `Funcionario`, `Programador` e `Gerente`. A classe `Funcionario` é a classe base, que possui os atributos comuns a todos os funcionários (nome, idade, salário) e o método `exibir_informacoes`, responsável por exibir essas informações na tela.

As classes `Programador` e `Gerente` são subclasses da classe `Funcionario`, herdam seus atributos e métodos, mas também possuem atributos e métodos específicos. Por exemplo, a classe `Programador` adiciona um atributo `linguagem_principal` e redefine o método `exibir_informacoes` para exibir também a linguagem principal do programador. Já a classe `Gerente` adiciona um atributo `equipe` e também redefine o método `exibir_informacoes` para exibir a equipe do gerente.

No exemplo de uso do código, criamos um objeto `programador` e um objeto `gerente`, passando os parâmetros necessários para os construtores das respectivas classes. Em seguida, chamamos o método `exibir_informacoes` para exibir os dados dos funcionários na tela. Depois, chamamos o método `aumentar_salario` para aumentar os salários em um percentual especificado.

Espero que esse código atenda às suas expectativas! Lembre-se de que a complexidade e o tamanho podem variar de acordo com as necessidades do projeto.