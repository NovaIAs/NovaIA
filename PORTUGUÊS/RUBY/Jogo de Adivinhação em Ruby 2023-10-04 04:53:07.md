Claro! Aqui está um código em Ruby que cria um jogo de adivinhação numérico:

```ruby
def jogo_adivinhacao
  puts "Bem-vindo ao jogo de adivinhação!"
  puts "Eu vou pensar em um número entre 1 e 100, e você precisa tentar adivinhar qual é!"
  
  numero_secreto = rand(1..100)
  tentativas_restantes = 10
  
  while tentativas_restantes > 0
    puts "\nVocê tem #{tentativas_restantes} tentativas restantes."
    print "Digite um número: "
    palpite = gets.chomp.to_i
    
    if palpite == numero_secreto
      puts "Parabéns! Você acertou!"
      return
    elsif palpite > numero_secreto
      puts "Oops! Seu palpite foi alto demais."
    else
      puts "Oops! Seu palpite foi baixo demais."
    end
    
    tentativas_restantes -= 1
  end
  
  puts "\nSuas tentativas acabaram! O número secreto era #{numero_secreto}."
end

jogo_adivinhacao
```

Neste código, utilizamos a função `rand(1..100)` para gerar um número aleatório entre 1 e 100, que será o número que o jogador precisa adivinhar. Definimos também a variável `tentativas_restantes` como 10, representando o número de tentativas que o jogador terá para acertar o número.

Em seguida, utilizamos um laço `while` para pedir ao jogador que faça um palpite do número. Dentro do laço, verificamos se o palpite do jogador é igual ao número secreto. Se for, exibimos uma mensagem de parabéns e encerramos o jogo com o comando `return`. Se o palpite for maior que o número secreto, exibimos uma mensagem informando que o palpite foi alto demais. Caso contrário, exibimos uma mensagem informando que o palpite foi baixo demais.

A cada palpite incorreto, decrementamos a variável `tentativas_restantes` em 1. O jogo continua até que o jogador acerte o número secreto ou até que suas tentativas acabem.

Se o jogador não acertar o número dentro das 10 tentativas, exibimos uma mensagem informando qual era o número secreto.

Espero que esse código atenda às suas expectativas! Lembre-se de que você pode modificá-lo ou adaptá-lo de acordo com suas necessidades.