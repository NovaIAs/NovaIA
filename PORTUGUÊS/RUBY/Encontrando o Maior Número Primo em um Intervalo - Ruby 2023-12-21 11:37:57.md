Claro, vou criar um código complexo em Ruby para você. Aqui está um exemplo de um algoritmo para encontrar o maior número primo em um intervalo específico:

```ruby
def eh_primo?(numero)
  return false if numero < 2

  (2..Math.sqrt(numero).to_i).each do |divisor|
    return false if numero % divisor == 0
  end

  true
end

def maior_primo(intervalo_inicial, intervalo_final)
  maior_primo_encontrado = 0

  (intervalo_inicial..intervalo_final).each do |numero|
    if eh_primo?(numero) && numero > maior_primo_encontrado
      maior_primo_encontrado = numero
    end
  end

  maior_primo_encontrado
end

puts "Digite o intervalo inicial:"
intervalo_inicial = gets.chomp.to_i

puts "Digite o intervalo final:"
intervalo_final = gets.chomp.to_i

maior_primo = maior_primo(intervalo_inicial, intervalo_final)

puts "O maior número primo encontrado entre #{intervalo_inicial} e #{intervalo_final} é: #{maior_primo}"
```

Neste código, temos duas funções principais: `eh_primo?` e `maior_primo`. 

A função `eh_primo?` verifica se um número é primo. Ela começa verificando se o número é menor que 2, pois números menores que 2 não são considerados primos. Em seguida, ela itera de 2 até a raiz quadrada do número (convertida para um número inteiro) e verifica se o número é divisível por algum dos divisores. Se for, o número não é primo e a função retorna `false`. Caso contrário, o número é primo e a função retorna `true`.

A função `maior_primo` recebe um intervalo inicial e um intervalo final como parâmetros. Ela inicializa a variável `maior_primo_encontrado` com 0. Em seguida, itera de `intervalo_inicial` até `intervalo_final`, verificando se cada número é primo utilizando a função `eh_primo?`. Se o número for primo e maior que o `maior_primo_encontrado` atual, o `maior_primo_encontrado` é atualizado para esse novo número. No final, a função retorna o `maior_primo_encontrado`.

Por fim, o programa principal solicita ao usuário que digite o intervalo inicial e o intervalo final. Em seguida, chama a função `maior_primo` passando os valores digitados e exibe o maior número primo encontrado no intervalo fornecido.