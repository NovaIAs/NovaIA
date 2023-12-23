Claro! Vou criar um código em Ruby que simula um jogo de adivinhação. O objetivo do jogo é adivinhar um número aleatório gerado pelo programa. Vou adicionar algumas funcionalidades extras para torná-lo mais interessante.

```ruby
class JogoAdivinhacao
  def initialize
    @pontuacao = 0
    @tentativas = 0
    iniciar_jogo
  end

  def iniciar_jogo
    puts "Bem-vindo ao Jogo de Adivinhação!"
    puts "Digite o seu nome: "
    nome = gets.chomp
    puts "Olá, #{nome}! Vamos começar o jogo!"
    puts "Eu vou gerar um número entre 1 e 100. Tente adivinhar qual é!"
    puts "Após cada tentativa, eu informarei se o número que você digitou é maior ou menor. Boa sorte!"
    gerar_numero_aleatorio
    jogar
  end

  def gerar_numero_aleatorio
    @numero_aleatorio = rand(1..100)
  end

  def jogar
    loop do
      print "Digite um número: "
      numero = gets.chomp.to_i
      @tentativas += 1

      if numero > @numero_aleatorio
        puts "O número é menor!"
      elsif numero < @numero_aleatorio
        puts "O número é maior!"
      else
        puts "Parabéns! Você acertou o número em #{@tentativas} tentativas!"
        atualizar_pontuacao
        break
      end
    end

    jogar_novamente
  end

  def atualizar_pontuacao
    @pontuacao += 1
    puts "Pontuação atual: #{@pontuacao}"
  end

  def jogar_novamente
    print "Deseja jogar novamente? (s/n): "
    resposta = gets.chomp.downcase

    if resposta == "s"
      @tentativas = 0
      gerar_numero_aleatorio
      jogar
    else
      puts "Obrigado por jogar! Sua pontuação final foi #{@pontuacao}."
    end
  end
end

jogo = JogoAdivinhacao.new
```

Neste código, criamos uma classe `JogoAdivinhacao`. Ao inicializá-la, o jogo começa. O jogador é solicitado a digitar seu nome e, em seguida, um número aleatório entre 1 e 100 é gerado. O jogador deve tentar adivinhar o número digitando valores. Após cada tentativa, o programa informa se o número é maior ou menor.

Quando o jogador acerta o número, a pontuação é atualizada e é dada a opção de jogar novamente. Se o jogador decidir jogar novamente, a pontuação é mantida e um novo número aleatório é gerado. Caso contrário, o programa encerra e exibe a pontuação final.

Espero que este código tenha atendido às suas expectativas!