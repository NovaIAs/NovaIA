Claro! Aqui está um exemplo de código em Ruby que implementa uma aplicação de gerenciamento de contatos:

```ruby
class Contato
  attr_accessor :nome, :telefone, :email

  def initialize(nome, telefone, email)
    @nome = nome
    @telefone = telefone
    @email = email
  end

  def to_s
    "#{nome}, #{telefone}, #{email}"
  end
end

class Agenda
  attr_accessor :contatos

  def initialize
    @contatos = []
  end

  def adicionar_contato
    print "Nome: "
    nome = gets.chomp
    print "Telefone: "
    telefone = gets.chomp
    print "Email: "
    email = gets.chomp

    contato = Contato.new(nome, telefone, email)
    contatos << contato

    puts "Contato adicionado com sucesso!"
  end

  def buscar_contato
    print "Digite o nome do contato: "
    nome = gets.chomp

    contatos_encontrados = contatos.select { |contato| contato.nome.downcase.include?(nome.downcase) }

    if contatos_encontrados.empty?
      puts "Nenhum contato encontrado."
    else
      puts "Contatos encontrados:"
      contatos_encontrados.each { |contato| puts contato }
    end
  end

  def listar_contatos
    if contatos.empty?
      puts "Nenhum contato cadastrado."
    else
      puts "Lista de contatos:"
      contatos.each { |contato| puts contato }
    end
  end

  def remover_contato
    print "Digite o nome do contato: "
    nome = gets.chomp

    contato_removido = contatos.find { |contato| contato.nome.downcase == nome.downcase }

    if contato_removido.nil?
      puts "Contato não encontrado."
    else
      contatos.delete(contato_removido)
      puts "Contato removido com sucesso!"
    end
  end
end

agenda = Agenda.new

loop do
  puts "\nEscolha uma opção:"
  puts "1. Adicionar contato"
  puts "2. Buscar contato"
  puts "3. Listar contatos"
  puts "4. Remover contato"
  puts "5. Sair"

  opcao = gets.chomp.to_i

  case opcao
  when 1
    agenda.adicionar_contato
  when 2
    agenda.buscar_contato
  when 3
    agenda.listar_contatos
  when 4
    agenda.remover_contato
  when 5
    break
  else
    puts "Opção inválida."
  end
end
```

Neste código, temos duas classes principais: `Contato` e `Agenda`. A classe `Contato` representa um contato individual com atributos como nome, telefone e email. A classe `Agenda` é responsável por gerenciar a lista de contatos.

A classe `Agenda` possui métodos para adicionar, buscar, listar e remover contatos. No método `adicionar_contato`, o usuário é solicitado a inserir as informações de um novo contato, que é então criado e adicionado à lista de contatos. No método `buscar_contato`, o usuário pode pesquisar contatos por nome, e a lista de contatos é filtrada para exibir apenas os contatos que correspondem à pesquisa. O método `listar_contatos` exibe todos os contatos cadastrados, enquanto o método `remover_contato` permite ao usuário remover um contato específico pelo nome.

O programa principal é um loop que exibe um menu de opções para o usuário. Dependendo da opção selecionada, o programa chama os métodos correspondentes da classe `Agenda`. O loop continua até que o usuário selecione a opção de sair.

Espero que este código atenda às suas expectativas!