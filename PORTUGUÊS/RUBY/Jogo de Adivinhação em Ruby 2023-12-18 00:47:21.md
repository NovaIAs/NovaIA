Claro! Aqui está um código complexo em Ruby que cria um jogo de adivinhação, onde o jogador precisa adivinhar um número secreto:

```ruby
def jogo_adivinhacao
  puts "Bem-vindo ao jogo de adivinhação!"
  puts "Estou pensando em um número de 1 a 100."
  puts "Tente adivinhar qual é o número!"

  numero_secreto = rand(1..100)
  tentativas = 0

  loop do
    print "Digite um número: "
    palpite = gets.chomp.to_i
    tentativas += 1

    if palpite == numero_secreto
      puts "Parabéns, você acertou o número em #{tentativas} tentativas!"
      break
    elsif palpite < numero_secreto
      puts "O número é maior que #{palpite}. Tente novamente!"
    else
      puts "O número é menor que #{palpite}. Tente novamente!"
    end
  end
end

jogo_adivinhacao
```

Explicando o código:

1. A função `jogo_adivinhacao` é definida no início do código. Ela é responsável por controlar o jogo de adivinhação.

2. Dentro da função, as mensagens de boas-vindas são exibidas ao jogador, explicando as regras do jogo.

3. A variável `numero_secreto` é inicializada com um número aleatório entre 1 e 100, gerado pela função `rand(1..100)`.

4. A variável `tentativas` é inicializada com 0 para contar quantas tentativas o jogador fez.

5. Em seguida, um loop é iniciado usando a palavra-chave `loop do`. Esse loop continuará até que o jogador acerte o número secreto e saia do loop usando a instrução `break`.

6. Dentro do loop, o jogador é solicitado a digitar um número usando a função `gets.chomp.to_i`, que lê a entrada do usuário e converte para um número inteiro.

7. O número de tentativas é incrementado em 1 usando o operador `+=`.

8. Em seguida, é feita uma série de verificações para determinar se o palpite do jogador é igual, maior ou menor do que o número secreto.

9. Se o palpite for igual ao número secreto, uma mensagem de parabéns é exibida, informando o número de tentativas que o jogador fez. O loop é interrompido usando o `break`.

10. Se o palpite for menor que o número secreto, uma mensagem é exibida informando que o número é maior e o jogador é solicitado a tentar novamente.

11. Se o palpite for maior que o número secreto, uma mensagem é exibida informando que o número é menor e o jogador é solicitado a tentar novamente.

12. O jogo continua até que o jogador acerte o número secreto e saia do loop.

13. No final do código, a função `jogo_adivinhacao` é chamada para iniciar o jogo.