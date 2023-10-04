Claro, vou criar um código em Ruby complexo e diferenciado para você. 

```ruby
# Classe para representar um animal
class Animal
  attr_accessor :nome, :idade
  
  def initialize(nome, idade)
    @nome = nome
    @idade = idade
  end
  
  def exibir_informacoes
    puts "Nome: #{@nome}"
    puts "Idade: #{@idade} anos"
  end
end

# Classe para representar um cachorro
class Cachorro < Animal
  attr_accessor :raca
  
  def initialize(nome, idade, raca)
    super(nome, idade)
    @raca = raca
  end
  
  def latir
    puts "O cachorro #{@nome} está latindo!"
  end
end

# Classe para representar um gato
class Gato < Animal
  attr_accessor :cor
  
  def initialize(nome, idade, cor)
    super(nome, idade)
    @cor = cor
  end
  
  def miar
    puts "O gato #{@nome} está miando!"
  end
end

# Classe para representar um pássaro
class Passaro < Animal
  attr_accessor :especie
  
  def initialize(nome, idade, especie)
    super(nome, idade)
    @especie = especie
  end
  
  def voar
    puts "O pássaro #{@nome} está voando!"
  end
end

# Método para criar um novo animal
def criar_animal
  puts "Qual o tipo de animal que você deseja criar? (1 - Cachorro, 2 - Gato, 3 - Pássaro)"
  tipo = gets.chomp.to_i
  
  puts "Digite o nome do animal:"
  nome = gets.chomp
  
  puts "Digite a idade do animal:"
  idade = gets.chomp.to_i
  
  case tipo
  when 1
    puts "Digite a raça do cachorro:"
    raca = gets.chomp
    return Cachorro.new(nome, idade, raca)
  when 2
    puts "Digite a cor do gato:"
    cor = gets.chomp
    return Gato.new(nome, idade, cor)
  when 3
    puts "Digite a espécie do pássaro:"
    especie = gets.chomp
    return Passaro.new(nome, idade, especie)
  else
    puts "Opção inválida!"
    return nil
  end
end

# Criar um novo animal
animal = criar_animal

# Exibir informações do animal criado
if animal.nil?
  puts "Nenhum animal foi criado!"
else
  animal.exibir_informacoes
  
  if animal.is_a?(Cachorro)
    animal.latir
  elsif animal.is_a?(Gato)
    animal.miar
  elsif animal.is_a?(Passaro)
    animal.voar
  end
end
```

Neste código em Ruby, criei uma hierarquia de classes para representar diferentes tipos de animais: Cachorro, Gato e Pássaro. Cada classe herda da classe base Animal, que possui os atributos comuns a todos os animais (nome e idade). Cada classe também possui seus próprios atributos específicos (raça para Cachorro, cor para Gato e espécie para Pássaro).

Além disso, criei um método `criar_animal` que permite ao usuário criar um novo animal, informando o tipo (1 para Cachorro, 2 para Gato e 3 para Pássaro), nome, idade e os atributos específicos de cada tipo de animal. O método retorna o objeto da classe correspondente ao tipo de animal criado.

Por fim, o código cria um novo animal utilizando o método `criar_animal` e exibe suas informações utilizando o método `exibir_informacoes`. Dependendo do tipo de animal criado, também é chamado o método correspondente a uma ação específica do animal (latir para Cachorro, miar para Gato e voar para Pássaro).

Espero que este código atenda às suas expectativas!