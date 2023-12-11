Claro! Aqui está um código em Ruby que utiliza programação orientada a objetos para simular um jogo de RPG simples:

```ruby
# Classe Personagem representa um personagem do jogo
class Personagem
  attr_accessor :nome, :classe, :nivel, :vida, :ataque, :defesa

  def initialize(nome, classe)
    @nome = nome
    @classe = classe
    @nivel = 1
    @vida = 100
    @ataque = 10
    @defesa = 5
  end

  def aumentar_nivel
    @nivel += 1
    @vida += 10
    @ataque += 5
    @defesa += 2
  end

  def atacar(outro_personagem)
    dano = @ataque - outro_personagem.defesa
    if dano > 0
      outro_personagem.vida -= dano
      puts "#{@nome} atacou #{outro_personagem.nome} causando #{dano} de dano."
    else
      puts "#{@nome} atacou #{outro_personagem.nome}, mas não causou dano."
    end
  end
end

# Criação dos personagens
personagem1 = Personagem.new("Guerreiro", "Guerreiro")
personagem2 = Personagem.new("Mago", "Mago")

# Simulação de um combate
puts "Iniciando combate:"
puts "#{personagem1.nome} (#{personagem1.classe}) vs #{personagem2.nome} (#{personagem2.classe})"

while personagem1.vida > 0 && personagem2.vida > 0
  personagem1.atacar(personagem2)
  personagem2.atacar(personagem1)
  puts "Status:"
  puts "#{personagem1.nome}: #{personagem1.vida} de vida"
  puts "#{personagem2.nome}: #{personagem2.vida} de vida"
end

if personagem1.vida > 0
  puts "#{personagem1.nome} venceu!"
else
  puts "#{personagem2.nome} venceu!"
end
```

Neste código, temos a classe `Personagem` que representa os personagens do jogo. Cada personagem possui atributos como nome, classe, nível, vida, ataque e defesa. O método `initialize` é responsável por inicializar esses atributos com valores padrão.

O método `aumentar_nivel` é chamado quando um personagem sobe de nível, aumentando seus atributos de acordo. O método `atacar` permite que um personagem ataque outro, calculando o dano com base no seu atributo de ataque e na defesa do outro personagem.

Na parte final do código, criamos dois personagens (`personagem1` e `personagem2`) e simulamos um combate entre eles. O combate continua até que a vida de um dos personagens chegue a zero. No final, exibimos o resultado do combate.

Este código é apenas um exemplo de como um jogo de RPG simples pode ser implementado em Ruby usando orientação a objetos. É possível adicionar mais funcionalidades, como habilidades especiais, itens, mapas, entre outros.