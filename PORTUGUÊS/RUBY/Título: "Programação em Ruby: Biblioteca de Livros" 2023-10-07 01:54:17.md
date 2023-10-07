Claro! Aqui está um exemplo de código complexo em Ruby:

```ruby
# Classe para representar um livro
class Livro
  attr_accessor :titulo, :autor, :ano_publicacao

  def initialize(titulo, autor, ano_publicacao)
    @titulo = titulo
    @autor = autor
    @ano_publicacao = ano_publicacao
  end

  def to_s
    "#{titulo} - #{autor} (#{ano_publicacao})"
  end
end

# Classe para representar uma biblioteca
class Biblioteca
  attr_accessor :livros

  def initialize
    @livros = []
  end

  def adicionar_livro(livro)
    livros << livro
  end

  def buscar_por_autor(autor)
    resultados = livros.select { |livro| livro.autor == autor }
    resultados.each { |livro| puts livro.to_s }
  end

  def buscar_por_titulo(titulo)
    resultados = livros.select { |livro| livro.titulo.include?(titulo) }
    resultados.each { |livro| puts livro.to_s }
  end

  def buscar_por_ano(ano)
    resultados = livros.select { |livro| livro.ano_publicacao == ano }
    resultados.each { |livro| puts livro.to_s }
  end
end

# Criando alguns livros
livro1 = Livro.new("Dom Casmurro", "Machado de Assis", 1899)
livro2 = Livro.new("O Senhor dos Anéis", "J.R.R. Tolkien", 1954)
livro3 = Livro.new("1984", "George Orwell", 1949)

# Criando uma biblioteca
biblioteca = Biblioteca.new

# Adicionando livros à biblioteca
biblioteca.adicionar_livro(livro1)
biblioteca.adicionar_livro(livro2)
biblioteca.adicionar_livro(livro3)

# Buscando livros por autor
puts "Livros de Machado de Assis:"
biblioteca.buscar_por_autor("Machado de Assis")

# Buscando livros por título
puts "Livros com 'Anéis' no título:"
biblioteca.buscar_por_titulo("Anéis")

# Buscando livros por ano
puts "Livros publicados em 1949:"
biblioteca.buscar_por_ano(1949)
```

Este código em Ruby cria duas classes: `Livro` e `Biblioteca`. A classe `Livro` representa um livro com atributos como título, autor e ano de publicação. A classe `Biblioteca` representa uma coleção de livros e possui métodos para adicionar livros à biblioteca e realizar buscas por autor, título ou ano de publicação.

O código cria três livros diferentes: "Dom Casmurro" de Machado de Assis (publicado em 1899), "O Senhor dos Anéis" de J.R.R. Tolkien (publicado em 1954) e "1984" de George Orwell (publicado em 1949). Esses livros são adicionados à biblioteca.

Em seguida, são realizadas três buscas diferentes na biblioteca: uma por autor (Machado de Assis), uma por título (que contenha a palavra "Anéis") e uma por ano de publicação (1949). Os resultados das buscas são exibidos no console.

Espero que esse código complexo e diferenciado atenda às suas expectativas!