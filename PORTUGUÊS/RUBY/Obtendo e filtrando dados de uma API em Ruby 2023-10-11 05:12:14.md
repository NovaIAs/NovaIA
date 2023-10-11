Claro! Aqui está um código complexo em Ruby que realiza uma tarefa diferenciada:

```ruby
require 'net/http'
require 'json'

def obter_dados_api(url)
  uri = URI(url)
  response = Net::HTTP.get(uri)
  JSON.parse(response)
end

def filtrar_dados(dados)
  dados_selecionados = []
  dados.each do |dado|
    if dado['ativo'] && dado['valor'] > 1000
      dados_selecionados << dado
    end
  end
  dados_selecionados
end

def exibir_dados(dados)
  dados.each do |dado|
    puts "Ativo: #{dado['ativo']}"
    puts "Valor: #{dado['valor']}"
    puts "Data: #{dado['data']}"
    puts "---"
  end
end

def main
  url = 'https://api.example.com/dados'
  dados_api = obter_dados_api(url)
  dados_filtrados = filtrar_dados(dados_api)
  exibir_dados(dados_filtrados)
end

main
```

Neste código, estamos realizando uma tarefa de obter dados de uma API, filtrar esses dados com base em certas condições e, em seguida, exibir os dados filtrados na tela.

Primeiro, importamos as bibliotecas `net/http` e `json` para fazer a requisição HTTP e manipular a resposta em formato JSON. 

A função `obter_dados_api` recebe uma URL como argumento, cria uma instância de `URI` com base nessa URL e faz uma requisição GET usando `Net::HTTP.get`. Em seguida, a resposta é convertida de JSON para um objeto Ruby usando `JSON.parse` e retornada.

A função `filtrar_dados` recebe os dados obtidos da API como argumento e itera sobre cada dado. Se o campo `ativo` for verdadeiro e o valor for maior que 1000, o dado é selecionado e adicionado a uma lista `dados_selecionados`. No final, essa lista é retornada.

A função `exibir_dados` recebe os dados filtrados como argumento e itera sobre cada dado, exibindo na tela o estado do ativo, o valor e a data. 

Por fim, a função `main` é chamada para executar o código principal. É definida uma URL para a API, os dados são obtidos usando a função `obter_dados_api`, filtrados usando a função `filtrar_dados` e exibidos na tela usando a função `exibir_dados`.