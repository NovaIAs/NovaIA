Claro! Aqui está um código em Ruby que cria uma simulação de um jogo de RPG simples. O objetivo do jogo é lutar contra monstros e ganhar experiência para evoluir o personagem.

```
class Personagem
  attr_accessor :nome, :nivel, :experiencia, :vida, :ataque, :defesa

  def initialize(nome)
    @nome = nome
    @nivel = 1
    @experiencia = 0
    @vida = 100
    @ataque = 10
    @defesa = 5
  end

  def ganhar_experiencia(exp)
    @experiencia += exp
    if @experiencia >= 100
      evoluir
    else
      puts "#{@nome} ganhou #{exp} de experiência!"
    end
  end

  def evoluir
    @nivel += 1
    @vida += 10
    @ataque += 5
    @defesa += 2
    @experiencia = 0
    puts "#{@nome} evoluiu para o nível #{@nivel}!"
  end

  def atacar(monstro)
    dano = @ataque - monstro.defesa
    if dano > 0
      monstro.vida -= dano
      puts "#{@nome} atacou #{monstro.nome} causando #{dano} de dano!"
    else
      puts "#{@nome} atacou #{monstro.nome} mas não causou dano!"
    end
  end

  def morrer
    puts "#{@nome} morreu!"
  end
end

class Monstro
  attr_accessor :nome, :vida, :ataque, :defesa

  def initialize(nome, vida, ataque, defesa)
    @nome = nome
    @vida = vida
    @ataque = ataque
    @defesa = defesa
  end

  def atacar(personagem)
    dano = @ataque - personagem.defesa
    if dano > 0
      personagem.vida -= dano
      puts "#{@nome} atacou #{personagem.nome} causando #{dano} de dano!"
    else
      puts "#{@nome} atacou #{personagem.nome} mas não causou dano!"
    end
  end

  def morrer
    puts "#{@nome} morreu!"
  end
end

# Criando o personagem principal
personagem = Personagem.new("Herói")

# Criando os monstros
monstro1 = Monstro.new("Goblin", 50, 8, 2)
monstro2 = Monstro.new("Esqueleto", 70, 12, 4)

# Iniciando o combate
while personagem.vida > 0
  puts "-------------------------------------"
  puts "Vida do #{personagem.nome}: #{personagem.vida}"
  puts "Nível do #{personagem.nome}: #{personagem.nivel}"
  puts "Experiência do #{personagem.nome}: #{personagem.experiencia}"
  puts "-------------------------------------"

  puts "Escolha um monstro para atacar:"
  puts "1) #{monstro1.nome} (Vida: #{monstro1.vida})"
  puts "2) #{monstro2.nome} (Vida: #{monstro2.vida})"

  escolha = gets.chomp.to_i

  if escolha == 1
    personagem.atacar(monstro1)
    if monstro1.vida <= 0
      monstro1.morrer
      personagem.ganhar_experiencia(50)
    else
      monstro1.atacar(personagem)
    end
  elsif escolha == 2
    personagem.atacar(monstro2)
    if monstro2.vida <= 0
      monstro2.morrer
      personagem.ganhar_experiencia(70)
    else
      monstro2.atacar(personagem)
    end
  else
    puts "Escolha inválida! Tente novamente."
  end
end

personagem.morrer
puts "Fim de jogo!"
```

Este código cria duas classes: `Personagem` e `Monstro`. A classe `Personagem` representa o personagem controlado pelo jogador, enquanto a classe `Monstro` representa os inimigos que o personagem irá enfrentar.

O personagem tem atributos como nome, nível, experiência, vida, ataque e defesa. Ele pode ganhar experiência ao derrotar monstros, evoluindo quando alcançar 100 pontos de experiência. A cada evolução, seus atributos são aumentados.

Os monstros também têm atributos como nome, vida, ataque e defesa. Eles podem atacar o personagem, causando dano na vida dele.

O jogo se baseia em um loop onde o jogador escolhe um monstro para atacar. O combate ocorre até que a vida do personagem chegue a zero. Quando isso acontece, uma mensagem de "Fim de jogo!" é exibida.

Espero que este código atenda às suas expectativas!