Claro! Aqui está um exemplo de um código complexo em Ruby, com uma funcionalidade diferenciada e explicação detalhada:

```ruby
# Classe Pessoa
class Pessoa
  attr_accessor :nome, :idade
  
  def initialize(nome, idade)
    @nome = nome
    @idade = idade
  end
  
  def apresentar
    puts "Olá, meu nome é #{@nome} e tenho #{@idade} anos."
  end
end

# Classe Estudante que herda de Pessoa
class Estudante < Pessoa
  attr_accessor :curso
  
  def initialize(nome, idade, curso)
    super(nome, idade)
    @curso = curso
  end
  
  def apresentar
    puts "Olá, meu nome é #{@nome}, tenho #{@idade} anos e estou cursando #{@curso}."
  end
end

# Classe Professor que herda de Pessoa
class Professor < Pessoa
  attr_accessor :disciplina
  
  def initialize(nome, idade, disciplina)
    super(nome, idade)
    @disciplina = disciplina
  end
  
  def apresentar
    puts "Olá, meu nome é #{@nome}, tenho #{@idade} anos e leciono a disciplina de #{@disciplina}."
  end
end

# Criação de objetos
pessoa = Pessoa.new("João", 30)
estudante = Estudante.new("Maria", 20, "Engenharia")
professor = Professor.new("Carlos", 40, "Matemática")

# Chamada dos métodos apresentar
pessoa.apresentar
estudante.apresentar
professor.apresentar
```

Neste código em Ruby, criamos três classes: Pessoa, Estudante e Professor. A classe Pessoa é a classe base que contém os atributos "nome" e "idade", e o método "apresentar" para exibir as informações de uma pessoa.

A classe Estudante herda da classe Pessoa e adiciona o atributo "curso". Além disso, sobrescreve o método "apresentar" para incluir a informação do curso.

A classe Professor também herda da classe Pessoa e adiciona o atributo "disciplina". Da mesma forma, sobrescreve o método "apresentar" para incluir a informação da disciplina.

No final do código, criamos objetos das três classes e chamamos o método "apresentar" para exibir as informações de cada objeto.

Esse código demonstra a utilização de herança em Ruby, onde as subclasses Estudante e Professor herdam os atributos e métodos da classe base Pessoa, mas também possuem suas próprias características.