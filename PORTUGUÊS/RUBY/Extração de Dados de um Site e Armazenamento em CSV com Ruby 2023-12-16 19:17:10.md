Claro! Aqui está um código complexo em Ruby que envolve a manipulação de dados e a interação com APIs externas. O código realiza a extração de informações de um site, analisa os dados e os armazena em um arquivo CSV.

```ruby
require 'net/http'
require 'json'
require 'csv'

# Define a URL do site para extração de dados
url = URI.parse('https://www.exemplo.com/api/dados')

# Faz a requisição GET para obter os dados do site
response = Net::HTTP.get_response(url)

# Verifica se a resposta é válida
if response.code == '200'
  # Converte o conteúdo da resposta em JSON
  data = JSON.parse(response.body)

  # Inicializa um array vazio para armazenar os dados extraídos
  extracted_data = []

  # Itera sobre os registros obtidos
  data.each do |record|
    # Extrai as informações relevantes de cada registro
    id = record['id']
    nome = record['nome']
    idade = record['idade']

    # Realiza alguma manipulação nos dados, se necessário
    # Por exemplo, podemos calcular a data de nascimento a partir da idade
    data_nascimento = Time.now.year - idade.to_i

    # Adiciona as informações extraídas ao array
    extracted_data << [id, nome, idade, data_nascimento]
  end

  # Define o nome do arquivo CSV de saída
  output_file = 'dados_extraidos.csv'

  # Abre o arquivo CSV em modo de escrita
  CSV.open(output_file, 'w') do |csv|
    # Escreve o cabeçalho do arquivo
    csv << ['ID', 'Nome', 'Idade', 'Data de Nascimento']

    # Escreve os dados extraídos no arquivo
    extracted_data.each do |record|
      csv << record
    end
  end

  puts "Os dados foram extraídos e armazenados em #{output_file} com sucesso!"
else
  puts "Ocorreu um erro ao obter os dados do site."
end
```

Neste código, utilizamos a biblioteca `net/http` para realizar a requisição HTTP GET para o site desejado. Em seguida, verificamos se a resposta foi bem-sucedida (código de resposta 200).

Caso a resposta seja válida, utilizamos a biblioteca `json` para converter o conteúdo da resposta em um objeto JSON. Em seguida, iteramos sobre os registros obtidos e extraímos as informações relevantes de cada registro.

Após a extração dos dados, podemos realizar manipulações adicionais, se necessário. Neste exemplo, calculamos a data de nascimento a partir da idade.

Finalmente, utilizamos a biblioteca `csv` para criar um arquivo CSV de saída. Abrimos o arquivo em modo de escrita e escrevemos o cabeçalho e os dados extraídos. O nome do arquivo de saída é definido na variável `output_file`.

Ao final do processo, exibimos uma mensagem de sucesso ou de erro, dependendo do resultado da extração e armazenamento dos dados.

Este código pode ser adaptado para diferentes sites e APIs, basta substituir a URL do site e ajustar a lógica de extração de dados de acordo com a estrutura dos registros obtidos.