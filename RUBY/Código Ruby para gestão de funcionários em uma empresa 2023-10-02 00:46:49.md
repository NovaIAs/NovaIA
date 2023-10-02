Claro! Vou gerar um código complexo em Ruby para você. Aqui está:

```ruby
# Classe Pessoa
class Pessoa
  attr_accessor :nome, :idade

  def initialize(nome, idade)
    @nome = nome
    @idade = idade
  end

  def saudacao
    if @idade < 18
      puts "Olá, #{@nome}! Você é menor de idade."
    else
      puts "Olá, #{@nome}! Você é maior de idade."
    end
  end
end

# Classe Funcionario
class Funcionario < Pessoa
  attr_accessor :cargo, :salario

  def initialize(nome, idade, cargo, salario)
    super(nome, idade)
    @cargo = cargo
    @salario = salario
  end

  def info_funcionario
    puts "Nome: #{@nome}"
    puts "Idade: #{@idade}"
    puts "Cargo: #{@cargo}"
    puts "Salário: #{@salario}"
  end
end

# Classe Empresa
class Empresa
  attr_accessor :nome, :funcionarios

  def initialize(nome)
    @nome = nome
    @funcionarios = []
  end

  def contratar(funcionario)
    @funcionarios << funcionario
  end

  def listar_funcionarios
    puts "Funcionários da #{@nome}:"
    @funcionarios.each do |funcionario|
      puts funcionario.nome
    end
  end
end

# Criando objetos
joao = Funcionario.new("João", 25, "Desenvolvedor", 5000)
maria = Funcionario.new("Maria", 30, "Gerente", 8000)

empresa = Empresa.new("Empresa ABC")
empresa.contratar(joao)
empresa.contratar(maria)

# Executando métodos
joao.saudacao
joao.info_funcionario

maria.saudacao
maria.info_funcionario

empresa.listar_funcionarios
```

Este código cria três classes: Pessoa, Funcionario e Empresa. A classe Pessoa possui os atributos nome e idade, além de um método de saudação que verifica se a pessoa é maior ou menor de idade. A classe Funcionario herda da classe Pessoa e possui os atributos cargo e salário, além de um método para exibir as informações do funcionário. A classe Empresa possui os atributos nome e funcionarios (uma lista de funcionários), além de métodos para contratar e listar funcionários.

Espero que goste deste código complexo em Ruby!