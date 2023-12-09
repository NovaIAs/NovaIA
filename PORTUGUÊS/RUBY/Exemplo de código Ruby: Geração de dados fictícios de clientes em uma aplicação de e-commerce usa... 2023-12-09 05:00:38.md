Claro! Aqui está um exemplo de código Ruby complexo, que utiliza a biblioteca "Faker" para gerar dados fictícios de clientes em uma aplicação de e-commerce. O código cria uma classe chamada "Cliente" e implementa várias funcionalidades, como a geração de dados aleatórios para nome, endereço, telefone e e-mail do cliente:

```ruby
require 'faker'

class Cliente
  attr_accessor :nome, :endereco, :telefone, :email

  def initialize
    @nome = Faker::Name.name
    @endereco = Faker::Address.full_address
    @telefone = Faker::PhoneNumber.phone_number
    @email = Faker::Internet.email
  end

  def self.gerar_clientes(numero)
    clientes = []
    numero.times { clientes << Cliente.new }
    clientes
  end

  def self.imprimir_dados(clientes)
    clientes.each do |cliente|
      puts "Nome: #{cliente.nome}"
      puts "Endereço: #{cliente.endereco}"
      puts "Telefone: #{cliente.telefone}"
      puts "E-mail: #{cliente.email}"
      puts "-------------------------"
    end
  end
end

# Gerar 5 clientes aleatórios
clientes = Cliente.gerar_clientes(5)

# Imprimir os dados dos clientes gerados
Cliente.imprimir_dados(clientes)
```

Neste código, a classe `Cliente` possui os atributos `nome`, `endereco`, `telefone` e `email`, que são gerados aleatoriamente usando os métodos da biblioteca `Faker`. 

O método `initialize` é chamado quando um novo objeto da classe `Cliente` é criado e atribui valores aleatórios aos atributos.

O método de classe `gerar_clientes` recebe um número como parâmetro e cria uma lista de clientes com base nesse número, utilizando o método `Cliente.new`.

Por fim, o método de classe `imprimir_dados` recebe a lista de clientes e imprime os dados de cada cliente na tela, utilizando a interpolação de strings para exibir as informações.

No exemplo final, o código gera 5 clientes aleatórios e imprime seus dados no console.