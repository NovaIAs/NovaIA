Claro! Aqui está um código complexo em Ruby, que envolve manipulação de arquivos, regex e cálculos matemáticos. Vou explicá-lo passo a passo:

```ruby
# Função para calcular o fatorial de um número
def fatorial(n)
  if n == 0
    return 1
  else
    return n * fatorial(n-1)
  end
end

# Função para encontrar todas as sequências numéricas em um arquivo de texto
def encontrar_sequencias_numericas(arquivo)
  sequencias = []
  File.open(arquivo, "r") do |file|
    file.each_line do |line|
      line.scan(/\d+/) do |match|
        sequencias << match.to_i
      end
    end
  end
  return sequencias
end

# Função para calcular a soma dos fatoriais de todos os números em uma sequência
def soma_fatoriais(sequencia)
  soma = 0
  sequencia.each do |num|
    soma += fatorial(num)
  end
  return soma
end

# Execução do programa
arquivo = "texto.txt"  # Substitua "texto.txt" pelo nome do arquivo que deseja processar

sequencias_numericas = encontrar_sequencias_numericas(arquivo)
total = soma_fatoriais(sequencias_numericas)

puts "A soma dos fatoriais dos números encontrados no arquivo #{arquivo} é #{total}."
```

Explicação do código:

1. Definimos uma função chamada `fatorial` que calcula o fatorial de um número usando recursão. Se o número for igual a zero, retornamos 1. Caso contrário, multiplicamos o número pelo fatorial do número anterior.
2. Em seguida, temos a função `encontrar_sequencias_numericas` que recebe o nome de um arquivo como parâmetro. Essa função abre o arquivo e lê linha por linha. Utilizamos uma expressão regular (`/\d+/`) para encontrar todas as sequências de números em cada linha. Cada sequência encontrada é convertida para inteiro e adicionada a um array chamado `sequencias`. No final, retornamos esse array.
3. A função `soma_fatoriais` recebe uma sequência de números como parâmetro. Utilizando um loop, calculamos o fatorial de cada número e somamos esses fatoriais em uma variável chamada `soma`. No final, retornamos o valor dessa soma.
4. Na parte de execução do programa, definimos o nome do arquivo que desejamos processar. Substitua `"texto.txt"` pelo nome do arquivo desejado.
5. Chamamos a função `encontrar_sequencias_numericas` passando o nome do arquivo como parâmetro e armazenamos o resultado na variável `sequencias_numericas`.
6. Em seguida, chamamos a função `soma_fatoriais` passando a variável `sequencias_numericas` como parâmetro e armazenamos o resultado na variável `total`.
7. Por fim, exibimos o resultado na tela utilizando `puts`.

Esse código lê um arquivo de texto, encontra todas as sequências numéricas presentes no arquivo, calcula o fatorial de cada número encontrado e soma esses fatoriais. O resultado final é exibido na tela.