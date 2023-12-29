Claro! Aqui está um código complexo em Ruby que utiliza alguns conceitos avançados e é explicado em detalhes:

```ruby
# Classe para representar um retângulo
class Retangulo
  attr_accessor :largura, :altura

  def initialize(largura, altura)
    @largura = largura
    @altura = altura
  end

  def calcular_area
    @largura * @altura
  end

  def calcular_perimetro
    2 * (@largura + @altura)
  end

  def redimensionar(nova_largura, nova_altura)
    @largura = nova_largura
    @altura = nova_altura
  end

  def desenhar
    for i in 1..@altura
      for j in 1..@largura
        if i == 1 || i == @altura || j == 1 || j == @largura
          print "*"
        else
          print " "
        end
      end
      puts
    end
  end
end

# Utilização da classe Retangulo para criar e manipular retângulos
retangulo = Retangulo.new(10, 5)

puts "Área do retângulo: #{retangulo.calcular_area}"
puts "Perímetro do retângulo: #{retangulo.calcular_perimetro}"

retangulo.redimensionar(8, 4)

puts "Nova área do retângulo: #{retangulo.calcular_area}"
puts "Novo perímetro do retângulo: #{retangulo.calcular_perimetro}"

puts "Desenho do retângulo:"
retangulo.desenhar
```

Neste código, temos uma classe chamada `Retangulo` que representa um retângulo. Ela possui atributos `largura` e `altura`, além de métodos para calcular a área e o perímetro do retângulo, redimensionar suas dimensões e desenhar um retângulo na tela.

O programa cria um objeto da classe `Retangulo` com largura 10 e altura 5, e em seguida imprime a área e o perímetro do retângulo. Em seguida, redimensiona o retângulo para largura 8 e altura 4, e imprime novamente a área e o perímetro.

Por fim, o programa exibe um desenho do retângulo na tela, utilizando asteriscos (*) para representar as bordas do retângulo e espaços em branco para representar o interior.

Espero que este código atenda às suas expectativas!