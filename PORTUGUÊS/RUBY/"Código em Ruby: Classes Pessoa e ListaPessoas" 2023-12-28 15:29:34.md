Claro! Aqui está um código complexo em Ruby:

```ruby
# Classe para representar uma pessoa
class Pessoa
  attr_accessor :nome, :idade

  def initialize(nome, idade)
    @nome = nome
    @idade = idade
  end

  def saudacao
    puts "Olá, meu nome é #{@nome} e tenho #{@idade} anos."
  end

  def aniversario
    @idade += 1
    puts "Parabéns! Agora tenho #{@idade} anos."
  end
end

# Classe para representar uma lista de pessoas
class ListaPessoas
  attr_accessor :pessoas

  def initialize
    @pessoas = []
  end

  def adicionar_pessoa(pessoa)
    @pessoas << pessoa
    puts "#{pessoa.nome} foi adicionado(a) à lista de pessoas."
  end

  def remover_pessoa(pessoa)
    @pessoas.delete(pessoa)
    puts "#{pessoa.nome} foi removido(a) da lista de pessoas."
  end

  def mostrar_pessoas
    puts "Lista de pessoas:"
    @pessoas.each do |pessoa|
      puts "- #{pessoa.nome}, #{pessoa.idade} anos"
    end
  end
end

# Instanciando algumas pessoas
pessoa1 = Pessoa.new("João", 25)
pessoa2 = Pessoa.new("Maria", 30)
pessoa3 = Pessoa.new("Pedro", 35)

# Instanciando a lista de pessoas
lista_pessoas = ListaPessoas.new

# Adicionando as pessoas à lista
lista_pessoas.adicionar_pessoa(pessoa1)
lista_pessoas.adicionar_pessoa(pessoa2)
lista_pessoas.adicionar_pessoa(pessoa3)

# Mostrando a lista de pessoas
lista_pessoas.mostrar_pessoas

# Removendo uma pessoa da lista
lista_pessoas.remover_pessoa(pessoa2)

# Mostrando a lista de pessoas novamente
lista_pessoas.mostrar_pessoas

# Fazendo aniversário
pessoa1.aniversario
pessoa3.aniversario

# Mostrando a lista de pessoas após o aniversário
lista_pessoas.mostrar_pessoas
```

Neste código, temos duas classes: `Pessoa` e `ListaPessoas`.

A classe `Pessoa` representa uma pessoa com os atributos `nome` e `idade`. Ela possui um método `saudacao` que imprime uma saudação com o nome e idade da pessoa, e um método `aniversario` que incrementa a idade em 1 e imprime uma mensagem de parabéns.

A classe `ListaPessoas` representa uma lista de pessoas. Ela possui um atributo `pessoas` que é um array vazio inicialmente. Ela possui métodos para adicionar uma pessoa à lista, remover uma pessoa da lista e mostrar a lista de pessoas.

No final do código, algumas pessoas são instanciadas usando a classe `Pessoa`, e em seguida, elas são adicionadas à lista de pessoas usando a classe `ListaPessoas`. A lista de pessoas é mostrada, uma pessoa é removida da lista, a lista é mostrada novamente e as pessoas fazem aniversário, com a lista sendo mostrada novamente no final.