Claro! Aqui está um código em Ruby que gera uma lista de números primos usando o Crivo de Eratóstenes:

```ruby
def encontrar_primos(n)
  # Inicializa um array com todos os números de 0 a n
  numeros = (0..n).to_a
  numeros[0] = numeros[1] = nil

  # Percorre o array
  numeros.each do |numero|
    next unless numero

    # Marca todos os múltiplos do número como não primos
    (numero * numero).step(n, numero) { |m| numeros[m] = nil }
  end

  # Retorna apenas os números primos
  numeros.compact
end

puts "Digite um número limite:"
limite = gets.chomp.to_i

primos = encontrar_primos(limite)
puts "Os números primos até #{limite} são:"
puts primos.join(", ")
```

Explicação do código:
1. A função `encontrar_primos` recebe um número `n` como entrada e retorna uma lista de números primos até `n`.
2. Inicializamos um array chamado `numeros` contendo todos os números de 0 a `n`.
3. Definimos os números 0 e 1 como `nil`, pois não são primos.
4. Percorremos o array `numeros` utilizando o método `each` e verificamos se o número atual é diferente de `nil`.
5. Para cada número primo encontrado, percorremos todos os seus múltiplos e marcamos-os como `nil`, pois eles não são primos.
6. Por fim, utilizamos o método `compact` para remover os valores `nil` do array `numeros` e retornamos a lista de números primos.
7. Solicitamos ao usuário que digite um número limite e convertemos a entrada para inteiro.
8. Chamamos a função `encontrar_primos` passando o limite informado e armazenamos o resultado na variável `primos`.
9. Exibimos os números primos encontrados utilizando o método `join` para concatenar os valores com uma vírgula.