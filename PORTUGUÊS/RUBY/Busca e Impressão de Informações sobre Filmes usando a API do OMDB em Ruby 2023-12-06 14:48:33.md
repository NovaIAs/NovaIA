Claro! Aqui está um código complexo em Ruby que busca e imprime informações sobre filmes usando a API do OMDB (Open Movie Database):

```ruby
require 'net/http'
require 'json'

def buscar_filme(titulo)
  url = URI.parse("http://www.omdbapi.com/?t=#{URI::encode(titulo)}&apikey=INSIRA_SUA_API_KEY_AQUI")
  req = Net::HTTP::Get.new(url.to_s)
  res = Net::HTTP.start(url.host, url.port) { |http| http.request(req) }
  
  if res.is_a?(Net::HTTPSuccess)
    return JSON.parse(res.body)
  else
    return nil
  end
end

def imprimir_detalhes(filme)
  puts "Título: #{filme['Title']}"
  puts "Ano: #{filme['Year']}"
  puts "Gênero: #{filme['Genre']}"
  puts "Diretor: #{filme['Director']}"
  puts "Atores: #{filme['Actors']}"
  puts "Sinopse: #{filme['Plot']}"
end

puts "Digite o título do filme que deseja buscar:"
titulo = gets.chomp

filme = buscar_filme(titulo)
if filme.nil?
  puts "Filme não encontrado!"
else
  imprimir_detalhes(filme)
end
```

Neste código, utilizamos as bibliotecas `net/http` e `json` para realizar requisições HTTP e manipular os dados retornados em formato JSON.

A função `buscar_filme` recebe um título como entrada e faz uma requisição à API do OMDB para buscar informações sobre o filme. Note que você precisará de uma chave de API válida para poder realizar as requisições corretamente. Você pode obter uma chave gratuita no site do OMDB.

A função `imprimir_detalhes` é responsável por exibir as informações do filme na tela. Ela utiliza os campos retornados pela API, como título, ano, gênero, diretor, atores e sinopse.

No final, o código solicita ao usuário que digite o título do filme a ser buscado. Em seguida, chama a função `buscar_filme` passando o título digitado como parâmetro. Se o filme for encontrado, os detalhes são impressos na tela; caso contrário, uma mensagem de erro é exibida.

Espero que este código atenda às suas expectativas e seja útil para você!