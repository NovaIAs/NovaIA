Claro! Aqui está um código em Ruby que cria um jogo de adivinhação onde o jogador tem que acertar um número aleatório dentro de um determinado intervalo:

```ruby
# Função para gerar um número aleatório dentro de um intervalo
def gerar_numero_aleatorio(min, max)
  rand(min..max)
end

# Função para verificar se o número digitado pelo jogador está correto
def verificar_palpite(palpite, numero_secreto)
  if palpite == numero_secreto
    puts "Parabéns! Você acertou o número secreto #{numero_secreto}!"
    return true
  elsif palpite < numero_secreto
    puts "O número secreto é maior que #{palpite}. Tente novamente."
  else
    puts "O número secreto é menor que #{palpite}. Tente novamente."
  end
  return false
end

# Função principal do jogo
def jogar_adivinhacao
  puts "Bem-vindo ao jogo de Adivinhação!"
  puts "Digite o valor mínimo do intervalo:"
  min = gets.chomp.to_i
  puts "Digite o valor máximo do intervalo:"
  max = gets.chomp.to_i

  numero_secreto = gerar_numero_aleatorio(min, max)
  tentativas = 0

  while true
    puts "\nTentativa #{tentativas + 1}:"
    puts "Digite o seu palpite:"
    palpite = gets.chomp.to_i
    tentativas += 1

    if verificar_palpite(palpite, numero_secreto)
      break
    end
  end

  puts "\nVocê utilizou #{tentativas} tentativas!"
end

# Início do jogo
jogar_adivinhacao()
```

Este código em Ruby cria um jogo de adivinhação onde o jogador precisa adivinhar um número secreto gerado aleatoriamente dentro de um intervalo definido pelo usuário.

A função `gerar_numero_aleatorio` recebe como parâmetros `min` e `max`, representando os valores mínimo e máximo do intervalo, e retorna um número aleatório dentro deste intervalo utilizando a função `rand`.

A função `verificar_palpite` recebe como parâmetros `palpite` (o número digitado pelo jogador) e `numero_secreto` (o número que o jogador precisa adivinhar). Ela verifica se o palpite é igual ao número secreto e retorna `true`, indicando que o jogador acertou. Caso contrário, ela informa se o número é maior ou menor que o palpite e retorna `false`.

A função `jogar_adivinhacao` é a função principal do jogo. Ela exibe uma mensagem de boas-vindas e solicita ao jogador que digite o valor mínimo e máximo do intervalo. Em seguida, ela gera o número secreto chamando a função `gerar_numero_aleatorio` e inicializa a variável `tentativas` como 0.

Dentro de um loop `while true`, o jogador é solicitado a digitar um palpite. O número de tentativas é incrementado a cada iteração. A função `verificar_palpite` é chamada para verificar se o palpite do jogador está correto. Se o jogador acertar, o loop é interrompido com a palavra-chave `break`.

Por fim, é exibida uma mensagem informando a quantidade de tentativas utilizadas pelo jogador.