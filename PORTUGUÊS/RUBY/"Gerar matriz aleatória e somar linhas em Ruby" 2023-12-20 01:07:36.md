Claro! Aqui está um código em Ruby que gera uma matriz aleatória e realiza a soma dos elementos de cada linha:

```ruby
def gerar_matriz(linhas, colunas)
  matriz = Array.new(linhas) { Array.new(colunas) }
  
  matriz.each_with_index do |linha, i|
    linha.each_with_index do |_, j|
      matriz[i][j] = rand(1..100)
    end
  end
  
  return matriz
end

def soma_linhas(matriz)
  linhas = matriz.length
  colunas = matriz[0].length
  soma_linhas = Array.new(linhas)
  
  for i in 0...linhas
    soma = 0
    for j in 0...colunas
      soma += matriz[i][j]
    end
    soma_linhas[i] = soma
  end
  
  return soma_linhas
end

# Exemplo de uso
linhas = 5
colunas = 5

matriz_gerada = gerar_matriz(linhas, colunas)
soma_linhas = soma_linhas(matriz_gerada)

puts "Matriz gerada:"
matriz_gerada.each { |linha| puts linha.to_s }

puts "\nSoma das linhas:"
soma_linhas.each { |soma| puts soma }
```

Explicação do código:

1. A função `gerar_matriz` recebe dois parâmetros `linhas` e `colunas` e retorna uma matriz preenchida com números aleatórios de 1 a 100. Para isso, é utilizado o método `Array.new` para criar uma matriz vazia com o número de linhas especificado. Em seguida, são percorridas todas as posições da matriz utilizando `each_with_index` para preencher cada elemento com um número aleatório gerado pela função `rand(1..100)`.

2. A função `soma_linhas` recebe uma matriz como parâmetro e retorna um array contendo a soma dos elementos de cada linha. Para isso, primeiro determinamos o número de linhas e colunas da matriz. Em seguida, é criado um array vazio `soma_linhas` para armazenar as somas. Utilizamos dois loops for para percorrer cada linha e coluna da matriz e somar os elementos. A soma de cada linha é armazenada no array `soma_linhas`.

3. No exemplo de uso, definimos o número de linhas e colunas da matriz e chamamos as funções `gerar_matriz` e `soma_linhas` para gerar a matriz aleatória e calcular as somas das linhas, respectivamente. Em seguida, imprimimos a matriz gerada e as somas das linhas utilizando `puts`.