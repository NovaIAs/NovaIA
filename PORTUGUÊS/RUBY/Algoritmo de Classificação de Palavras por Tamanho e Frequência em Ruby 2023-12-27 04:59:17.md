Claro! Aqui está um código complexo em Ruby que apresenta um algoritmo de classificação de palavras por tamanho e frequência, utilizando uma abordagem de contagem de palavras em um texto:

```ruby
def classificar_palavras(texto)
  # Dividir o texto em palavras
  palavras = texto.downcase.scan(/\w+/)

  # Contar a frequência de cada palavra
  frequencia = Hash.new(0)
  palavras.each { |palavra| frequencia[palavra] += 1 }

  # Classificar as palavras por tamanho e frequência
  classificacao = Hash.new { |h, k| h[k] = [] }
  palavras.each { |palavra| classificacao[palavra.size] << palavra }

  # Ordenar as palavras por tamanho e frequência
  classificacao.each do |tamanho, palavras|
    classificacao[tamanho] = palavras.sort_by { |palavra| -frequencia[palavra] }
  end

  # Retornar a classificação das palavras
  classificacao
end

# Texto de exemplo
texto = "Este é um exemplo de texto com algumas palavras repetidas. Algumas palavras aparecem mais vezes do que outras."

# Classificar as palavras do texto
resultado = classificar_palavras(texto)

# Imprimir a classificação das palavras
resultado.keys.sort.each do |tamanho|
  palavras = resultado[tamanho]
  puts "Palavras com tamanho #{tamanho}: #{palavras.join(', ')}"
end
```

Neste código, a função `classificar_palavras` recebe um texto como entrada e realiza as seguintes etapas:

1. Divide o texto em palavras, removendo pontuações e transformando todas as palavras em minúsculas.
2. Conta a frequência de cada palavra utilizando um hash.
3. Classifica as palavras por tamanho e armazena em um hash de listas, onde as chaves são os tamanhos das palavras e os valores são as listas de palavras com esse tamanho.
4. Ordena as palavras em cada lista por frequência decrescente.
5. Retorna o hash de classificação das palavras.

Após a chamada da função `classificar_palavras`, o código exibe a classificação das palavras, imprimindo as palavras de cada tamanho em ordem crescente de tamanho.