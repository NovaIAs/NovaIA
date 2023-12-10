Claro, vou lhe fornecer um exemplo de código complexo em Ruby que é diferenciado e grande. O código a seguir é um exemplo de um sistema de gerenciamento de biblioteca, onde é possível adicionar livros, pesquisar por título, autor ou gênero, bem como listar todos os livros disponíveis.

```ruby
class Livro
  attr_accessor :titulo, :autor, :genero

  def initialize(titulo, autor, genero)
    @titulo = titulo
    @autor = autor
    @genero = genero
  end
end

class Biblioteca
  def initialize
    @livros = []
  end

  def adicionar_livro(livro)
    @livros << livro
  end

  def pesquisar_por_titulo(titulo)
    resultados = []
    @livros.each do |livro|
      resultados << livro if livro.titulo.downcase.include?(titulo.downcase)
    end
    resultados
  end

  def pesquisar_por_autor(autor)
    resultados = []
    @livros.each do |livro|
      resultados << livro if livro.autor.downcase.include?(autor.downcase)
    end
    resultados
  end

  def pesquisar_por_genero(genero)
    resultados = []
    @livros.each do |livro|
      resultados << livro if livro.genero.downcase.include?(genero.downcase)
    end
    resultados
  end

  def listar_livros
    @livros.each do |livro|
      puts "Título: #{livro.titulo}"
      puts "Autor: #{livro.autor}"
      puts "Gênero: #{livro.genero}"
      puts "-" * 30
    end
  end
end

biblioteca = Biblioteca.new

livro1 = Livro.new("Harry Potter e a Pedra Filosofal", "J.K. Rowling", "Fantasia")
livro2 = Livro.new("O Senhor dos Anéis: A Sociedade do Anel", "J.R.R. Tolkien", "Fantasia")
livro3 = Livro.new("1984", "George Orwell", "Ficção Científica")
livro4 = Livro.new("Dom Casmurro", "Machado de Assis", "Romance")

biblioteca.adicionar_livro(livro1)
biblioteca.adicionar_livro(livro2)
biblioteca.adicionar_livro(livro3)
biblioteca.adicionar_livro(livro4)

puts "Pesquisar por título:"
resultados_titulo = biblioteca.pesquisar_por_titulo("harry potter")
resultados_titulo.each do |livro|
  puts "Título: #{livro.titulo}"
  puts "Autor: #{livro.autor}"
  puts "Gênero: #{livro.genero}"
  puts "-" * 30
end

puts "Pesquisar por autor:"
resultados_autor = biblioteca.pesquisar_por_autor("j.r.r. tolkien")
resultados_autor.each do |livro|
  puts "Título: #{livro.titulo}"
  puts "Autor: #{livro.autor}"
  puts "Gênero: #{livro.genero}"
  puts "-" * 30
end

puts "Pesquisar por gênero:"
resultados_genero = biblioteca.pesquisar_por_genero("romance")
resultados_genero.each do |livro|
  puts "Título: #{livro.titulo}"
  puts "Autor: #{livro.autor}"
  puts "Gênero: #{livro.genero}"
  puts "-" * 30
end

puts "Listar todos os livros:"
biblioteca.listar_livros
```

Neste código, temos duas classes: `Livro` e `Biblioteca`. A classe `Livro` possui os atributos `titulo`, `autor` e `genero`, e o método `initialize` para inicializar esses atributos.

A classe `Biblioteca` possui um array `@livros` para armazenar os livros adicionados. Ela possui os métodos `adicionar_livro` para adicionar um livro à biblioteca, `pesquisar_por_titulo` para buscar livros pelo título, `pesquisar_por_autor` para buscar livros pelo autor, `pesquisar_por_genero` para buscar livros pelo gênero e `listar_livros` para exibir todos os livros da biblioteca.

No exemplo, são criados alguns objetos da classe `Livro` e adicionados à biblioteca. Em seguida, são realizadas pesquisas por título, autor e gênero, além de mostrar todos os livros disponíveis na biblioteca.

Esse código complexo em Ruby é apenas um exemplo, e pode ser adaptado e expandido de acordo com as necessidades do sistema de gerenciamento de biblioteca.