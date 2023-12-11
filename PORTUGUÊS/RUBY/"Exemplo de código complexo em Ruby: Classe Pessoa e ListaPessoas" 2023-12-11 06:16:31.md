Claro! Aqui está um exemplo de código complexo em Ruby:

```ruby
# Classe para representar uma pessoa
class Pessoa
  # Atributos da classe
  attr_accessor :nome, :idade
  
  # Método construtor
  def initialize(nome, idade)
    @nome = nome
    @idade = idade
  end
  
  # Método para verificar se a pessoa é maior de idade
  def maior_de_idade?
    @idade >= 18
  end
  
  # Método para imprimir os detalhes da pessoa
  def imprimir_detalhes
    puts "Nome: #{@nome}"
    puts "Idade: #{@idade}"
    puts maior_de_idade? ? "Maior de idade" : "Menor de idade"
  end
end

# Classe para representar uma lista de pessoas
class ListaPessoas
  # Atributo da classe
  attr_accessor :pessoas
  
  # Método construtor
  def initialize
    @pessoas = []
  end
  
  # Método para adicionar uma pessoa à lista
  def adicionar_pessoa(pessoa)
    @pessoas << pessoa
  end
  
  # Método para imprimir os detalhes de todas as pessoas da lista
  def imprimir_detalhes_pessoas
    @pessoas.each(&:imprimir_detalhes)
  end
end

# Criação de algumas pessoas
pessoa1 = Pessoa.new("João", 25)
pessoa2 = Pessoa.new("Maria", 17)
pessoa3 = Pessoa.new("Pedro", 20)

# Criação da lista de pessoas
lista_pessoas = ListaPessoas.new

# Adição das pessoas à lista
lista_pessoas.adicionar_pessoa(pessoa1)
lista_pessoas.adicionar_pessoa(pessoa2)
lista_pessoas.adicionar_pessoa(pessoa3)

# Impressão dos detalhes de todas as pessoas da lista
lista_pessoas.imprimir_detalhes_pessoas
```

Neste exemplo, temos duas classes: `Pessoa` e `ListaPessoas`. A classe `Pessoa` representa uma pessoa com atributos como nome e idade, e possui métodos para verificar se a pessoa é maior de idade e imprimir seus detalhes. A classe `ListaPessoas` representa uma lista de pessoas, com um atributo que armazena as pessoas e métodos para adicionar pessoas à lista e imprimir os detalhes de todas as pessoas.

No código principal, são criadas três pessoas e adicionadas à lista de pessoas. Em seguida, os detalhes de todas as pessoas da lista são impressos.

Espero que este exemplo atenda às suas expectativas!