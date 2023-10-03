Claro! Aqui está um código em Ruby que cria um jogo de adivinhação de números. Ele utiliza um algoritmo de busca binária para tornar o processo mais eficiente. Vamos explicar o código em detalhes:

```ruby
def adivinhar_numero(numero)
  menor = 1
  maior = 100
  tentativas = 0

  while menor <= maior
    media = (menor + maior) / 2
    tentativas += 1

    if media == numero
      puts "Acertou! O número é #{media}."
      puts "Número de tentativas: #{tentativas}"
      return
    elsif media < numero
      menor = media + 1
    else
      maior = media - 1
    end
  end

  puts "Não encontrou o número #{numero}."
end

# Chamada da função
numero_para_adivinhar = rand(1..100)
puts "Tente adivinhar o número entre 1 e 100:"
adivinhar_numero(numero_para_adivinhar)
```

O código começa definindo uma função chamada `adivinhar_numero` que recebe um parâmetro `numero`, representando o número que o jogador precisa adivinhar.

Dentro da função, duas variáveis `menor` e `maior` são inicializadas com os valores extremos do intervalo onde o número pode estar, nesse caso, 1 e 100. A variável `tentativas` é inicializada com zero para contar o número de tentativas feitas pelo jogador.

Em seguida, um laço `while` é utilizado para repetir o processo de adivinhação até que o intervalo `menor` e `maior` se cruzem. Dentro do laço, a variável `media` é calculada como a média entre `menor` e `maior`. 

A cada iteração, o número de tentativas é incrementado em 1. Se a `media` for igual ao `numero` que está sendo procurado, o jogador acertou e uma mensagem é exibida com o número e o total de tentativas. A função retorna e o jogo termina.

Caso a `media` seja menor que o `numero`, significa que o número procurado está na segunda metade do intervalo. Nesse caso, a variável `menor` é atualizada para `media + 1`, descartando a primeira metade do intervalo. Caso contrário, se a `media` for maior que o `numero`, o número procurado está na primeira metade do intervalo e a variável `maior` é atualizada para `media - 1`, descartando a segunda metade.

Ao final do laço, se o número não for encontrado, uma mensagem é exibida informando que o número não foi encontrado.

Por fim, o jogo é iniciado com a geração aleatória de um número entre 1 e 100, que é armazenado na variável `numero_para_adivinhar`. O jogador é solicitado a tentar adivinhar o número e a função `adivinhar_numero` é chamada com o número gerado como argumento.