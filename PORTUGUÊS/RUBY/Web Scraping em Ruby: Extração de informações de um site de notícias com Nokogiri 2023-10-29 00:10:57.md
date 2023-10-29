Claro! Aqui está um código complexo em Ruby que utiliza a biblioteca 'nokogiri' para fazer web scraping e extrair informações de um site. O código irá coletar os dados de um site de notícias e salvá-los em um arquivo CSV:

```ruby
require 'nokogiri'
require 'open-uri'
require 'csv'

# Função para extrair as informações do site e salvar em um arquivo CSV
def extrair_informacoes(url, arquivo_csv)
  doc = Nokogiri::HTML(open(url))

  # Array para armazenar as informações extraídas
  informacoes = []

  # Extrair o título, descrição e link de cada notícia
  doc.css('.noticia').each_with_index do |noticia, index|
    titulo = noticia.css('.titulo').text.strip
    descricao = noticia.css('.descricao').text.strip
    link = noticia.css('a').attr('href').value

    # Adicionar as informações ao array
    informacoes << { "Título" => titulo, "Descrição" => descricao, "Link" => link }
  end

  # Salvar as informações em um arquivo CSV
  CSV.open(arquivo_csv, "w") do |csv|
    # Escrever o cabeçalho do arquivo CSV
    csv << informacoes.first.keys

    # Escrever cada linha de informações no arquivo CSV
    informacoes.each do |informacao|
      csv << informacao.values
    end
  end

  puts "As informações foram extraídas e salvas em #{arquivo_csv}!"
end

# URL do site de notícias
url = "https://www.exemplo.com/noticias"

# Nome do arquivo CSV para salvar as informações
arquivo_csv = "informacoes.csv"

# Chamar a função para extrair as informações e salvar no arquivo CSV
extrair_informacoes(url, arquivo_csv)
```

Neste código, começamos importando as bibliotecas 'nokogiri', 'open-uri' e 'csv'. A biblioteca 'nokogiri' é utilizada para fazer o parsing do HTML e a extração das informações desejadas. A biblioteca 'open-uri' é utilizada para abrir a URL do site de notícias. A biblioteca 'csv' é utilizada para salvar as informações em um arquivo CSV.

A seguir, definimos a função `extrair_informacoes`, que recebe a URL do site de notícias e o nome do arquivo CSV como parâmetros. Dentro da função, utilizamos o `Nokogiri` para fazer o parsing do HTML da página e selecionar os elementos desejados usando seletores CSS.

Usando o método `each_with_index`, percorremos cada elemento HTML com a classe `noticia` e extraímos o título, descrição e link de cada notícia. Em seguida, adicionamos essas informações a um array chamado `informacoes` na forma de um hash.

Depois de extrair todas as informações, utilizamos o `CSV.open` para criar um novo arquivo CSV e escrever as informações nele. Primeiro, escrevemos o cabeçalho do arquivo CSV, que são as chaves do primeiro elemento no array `informacoes`. Em seguida, escrevemos cada linha de informações no arquivo CSV, que são os valores de cada hash.

Por fim, chamamos a função `extrair_informacoes` passando a URL do site de notícias e o nome do arquivo CSV como argumentos. O código irá extrair as informações do site e salvá-las no arquivo CSV especificado.

Lembre-se de substituir a URL do site de notícias e o nome do arquivo CSV de acordo com suas necessidades.