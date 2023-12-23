Claro! Aqui está um exemplo de código complexo em Ruby:

```ruby
# Classe para representar uma pessoa
class Pessoa
  attr_accessor :nome, :idade

  def initialize(nome, idade)
    @nome = nome
    @idade = idade
  end

  def cumprimentar
    puts "Olá, meu nome é #{@nome} e tenho #{@idade} anos."
  end

  def calcular_aniversario
    ano_atual = Time.now.year
    ano_nascimento = ano_atual - @idade
    puts "Você nasceu no ano de #{ano_nascimento}."
  end
end

# Classe para representar uma lista de pessoas
class ListaPessoas
  def initialize
    @pessoas = []
  end

  def adicionar_pessoa(pessoa)
    @pessoas << pessoa
  end

  def listar_pessoas
    @pessoas.each do |pessoa|
      puts pessoa.nome
    end
  end
end

# Instanciando algumas pessoas
pessoa1 = Pessoa.new("João", 25)
pessoa2 = Pessoa.new("Maria", 30)
pessoa3 = Pessoa.new("Pedro", 40)

# Instanciando uma lista de pessoas
lista_pessoas = ListaPessoas.new

# Adicionando pessoas na lista
lista_pessoas.adicionar_pessoa(pessoa1)
lista_pessoas.adicionar_pessoa(pessoa2)
lista_pessoas.adicionar_pessoa(pessoa3)

# Listando as pessoas da lista
puts "Lista de pessoas:"
lista_pessoas.listar_pessoas

# Cumprimentando as pessoas da lista
puts "\nCumprimentos:"
lista_pessoas.listar_pessoas.each do |pessoa|
  pessoa.cumprimentar
end

# Calculando o ano de nascimento das pessoas da lista
puts "\nAno de nascimento:"
lista_pessoas.listar_pessoas.each do |pessoa|
  pessoa.calcular_aniversario
end
```

Neste código em Ruby, criamos duas classes: "Pessoa" e "ListaPessoas". A classe "Pessoa" possui atributos de nome e idade, um método para cumprimentar e um método para calcular o ano de nascimento com base na idade. A classe "ListaPessoas" é responsável por armazenar e gerenciar uma lista de pessoas.

No exemplo, criamos três instâncias da classe "Pessoa" e as adicionamos em uma instância da classe "ListaPessoas". Em seguida, listamos as pessoas da lista, cumprimentamos cada uma delas e calculamos seus anos de nascimento.

Espero que este exemplo atenda às suas necessidades!