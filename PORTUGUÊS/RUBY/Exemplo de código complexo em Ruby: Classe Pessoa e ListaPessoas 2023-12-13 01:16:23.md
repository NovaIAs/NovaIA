Claro! Aqui está um exemplo de código complexo em Ruby:

```ruby
# Classe que representa uma pessoa
class Pessoa
  attr_accessor :nome, :idade, :endereco

  def initialize(nome, idade, endereco)
    @nome = nome
    @idade = idade
    @endereco = endereco
  end

  def apresentar
    puts "Olá, meu nome é #{@nome}, tenho #{@idade} anos e moro em #{@endereco}."
  end

  def verificar_maioridade
    if @idade >= 18
      puts "#{@nome} é maior de idade."
    else
      puts "#{@nome} é menor de idade."
    end
  end
end

# Classe que representa uma lista de pessoas
class ListaPessoas
  def initialize
    @pessoas = []
  end

  def adicionar_pessoa(pessoa)
    @pessoas << pessoa
  end

  def remover_pessoa(pessoa)
    @pessoas.delete(pessoa)
  end

  def listar_pessoas
    @pessoas.each do |pessoa|
      pessoa.apresentar
    end
  end

  def listar_maiores_idade
    @pessoas.each do |pessoa|
      pessoa.verificar_maioridade
    end
  end
end

# Criação de instâncias da classe Pessoa
pessoa1 = Pessoa.new("João", 25, "Rua A")
pessoa2 = Pessoa.new("Maria", 17, "Rua B")
pessoa3 = Pessoa.new("Pedro", 35, "Rua C")

# Criação de instância da classe ListaPessoas
lista = ListaPessoas.new

# Adicionando pessoas à lista
lista.adicionar_pessoa(pessoa1)
lista.adicionar_pessoa(pessoa2)
lista.adicionar_pessoa(pessoa3)

# Listando informações das pessoas
puts "Lista de Pessoas:"
lista.listar_pessoas

# Verificando maiores de idade
puts "\nVerificação de Maiores de Idade:"
lista.listar_maiores_idade
```

Neste exemplo, temos duas classes: `Pessoa` e `ListaPessoas`. A classe `Pessoa` representa uma pessoa com atributos como nome, idade e endereço. Ela também possui métodos para se apresentar e verificar se é maior de idade.

A classe `ListaPessoas` representa uma lista de pessoas, onde é possível adicionar e remover pessoas, além de listar todas as pessoas e verificar se são maiores de idade.

No final do código, são criadas instâncias das classes `Pessoa` e `ListaPessoas`, adicionando pessoas à lista e realizando a listagem das informações e verificação de maiores de idade.