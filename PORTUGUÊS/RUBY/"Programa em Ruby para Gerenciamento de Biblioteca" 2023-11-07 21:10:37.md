Claro, vou criar um código complexo em Ruby e explicá-lo em detalhes. Vou criar um exemplo de um sistema de gerenciamento de biblioteca, que permitirá adicionar, remover e pesquisar livros. Vou também incluir uma interface de linha de comando para interagir com o sistema. Este é um exemplo de um código complexo que combina várias funcionalidades. Vamos começar:

```ruby
# Classe para representar um livro
class Livro
  attr_accessor :titulo, :autor, :ano
  
  def initialize(titulo, autor, ano)
    @titulo = titulo
    @autor = autor
    @ano = ano
  end

  def to_s
    "#{@titulo} (#{@autor}, #{@ano})"
  end
end

# Classe para representar a biblioteca
class Biblioteca
  def initialize
    @livros = []
  end

  def adicionar_livro(livro)
    @livros << livro
  end

  def remover_livro(titulo)
    @livros.delete_if { |livro| livro.titulo == titulo }
  end

  def buscar_livro(titulo)
    @livros.select { |livro| livro.titulo == titulo }
  end

  def listar_livros
    @livros.each do |livro|
      puts livro.to_s
    end
  end
end

# Função para exibir o menu de opções
def exibir_menu
  puts "1. Adicionar livro"
  puts "2. Remover livro"
  puts "3. Buscar livro"
  puts "4. Listar livros"
  puts "5. Sair"
end

# Programa principal
biblioteca = Biblioteca.new

loop do
  exibir_menu
  print "Escolha uma opção: "
  opcao = gets.chomp.to_i

  case opcao
  when 1
    print "Digite o título do livro: "
    titulo = gets.chomp
    print "Digite o autor do livro: "
    autor = gets.chomp
    print "Digite o ano do livro: "
    ano = gets.chomp.to_i
    livro = Livro.new(titulo, autor, ano)
    biblioteca.adicionar_livro(livro)
  when 2
    print "Digite o título do livro a ser removido: "
    titulo = gets.chomp
    biblioteca.remover_livro(titulo)
  when 3
    print "Digite o título do livro a ser buscado: "
    titulo = gets.chomp
    livros_encontrados = biblioteca.buscar_livro(titulo)
    if livros_encontrados.empty?
      puts "Nenhum livro encontrado com o título '#{titulo}'."
    else
      puts "Livros encontrados:"
      livros_encontrados.each do |livro|
        puts livro.to_s
      end
    end
  when 4
    puts "Lista de livros na biblioteca:"
    biblioteca.listar_livros
  when 5
    puts "Saindo do programa. Até mais!"
    break
  else
    puts "Opção inválida. Tente novamente."
  end
end
```

Este código em Ruby cria uma classe `Livro` para representar livros com título, autor e ano de publicação, e uma classe `Biblioteca` para gerenciar uma lista de livros. O programa principal permite adicionar, remover, buscar e listar livros na biblioteca por meio de uma interface de linha de comando.

Espero que este código seja útil e atenda às suas expectativas. Se você tiver alguma dúvida ou precisar de mais explicações sobre o código, fique à vontade para perguntar.