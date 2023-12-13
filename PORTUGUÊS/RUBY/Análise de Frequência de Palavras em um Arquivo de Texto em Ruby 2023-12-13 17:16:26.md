Claro! Aqui está um código complexo em Ruby, que realiza a leitura de um arquivo de texto, conta a frequência de cada palavra e exibe as palavras mais frequentes em ordem decrescente:

```ruby
def contar_palavras(arquivo)
  # Criando um hash para armazenar as palavras e suas frequências
  frequencia_palavras = Hash.new(0)

  # Lendo o arquivo de texto
  File.open(arquivo, 'r') do |file|
    file.each_line do |linha|
      # Removendo caracteres especiais e convertendo para letras minúsculas
      palavras = linha.downcase.gsub(/[^a-záàâãéèêíïóôõöúçñ]/, ' ').split

      # Incrementando a frequência de cada palavra
      palavras.each { |palavra| frequencia_palavras[palavra] += 1 }
    end
  end

  # Ordenando as palavras por frequência em ordem decrescente
  palavras_ordenadas = frequencia_palavras.sort_by { |_palavra, frequencia| -frequencia }

  # Exibindo as 10 palavras mais frequentes
  puts "As 10 palavras mais frequentes no arquivo #{arquivo}:"
  palavras_ordenadas.take(10).each_with_index do |(palavra, frequencia), indice|
    puts "#{indice + 1}. #{palavra}: #{frequencia} ocorrências"
  end
end

# Chamando a função com o nome do arquivo de texto como argumento
contar_palavras('texto.txt')
```

Neste código, a função `contar_palavras` recebe o nome de um arquivo de texto como parâmetro. Em seguida, é criado um hash chamado `frequencia_palavras` para armazenar as palavras e suas frequências.

O arquivo é lido linha por linha utilizando o método `each_line`, e cada linha é convertida para letras minúsculas e tem seus caracteres especiais removidos. Em seguida, as palavras são separadas utilizando o método `split`.

A frequência de cada palavra é incrementada utilizando o hash `frequencia_palavras`, onde a chave é a palavra e o valor é a frequência.

Depois de ler todo o arquivo, as palavras são ordenadas por frequência em ordem decrescente utilizando o método `sort_by`. Em seguida, as 10 palavras mais frequentes são exibidas utilizando o método `take(10)`.

Para executar o código, basta chamar a função `contar_palavras` passando o nome do arquivo de texto desejado como argumento.