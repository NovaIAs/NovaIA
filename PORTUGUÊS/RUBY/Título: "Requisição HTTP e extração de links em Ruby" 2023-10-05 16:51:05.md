Claro! Aqui está um código complexo em Ruby, que utiliza a biblioteca "net/http" para fazer uma requisição HTTP GET para um URL específico e retorna o conteúdo da página:

```ruby
require 'net/http'

def faz_requisicao(url)
  uri = URI(url)
  response = Net::HTTP.get_response(uri)

  if response.code.to_i == 200
    return response.body
  else
    return nil
  end
end

def extrai_links(html)
  links = html.scan(/<a\s+href="([^"]+)"/)
  links.flatten!
  links.uniq!

  return links
end

def main()
  url = "https://www.example.com"
  html = faz_requisicao(url)

  if html.nil?
    puts "Falha ao fazer a requisição para #{url}"
    return
  end

  links = extrai_links(html)

  puts "Links encontrados:"
  links.each do |link|
    puts link
  end
end

main()
```

Neste código, temos uma função chamada `faz_requisicao` que recebe uma URL como parâmetro, cria um objeto `URI` com a URL e utiliza a biblioteca `net/http` para fazer uma requisição HTTP GET. O código verifica se a resposta possui código 200 (indicando uma resposta OK) e retorna o conteúdo da página.

A função `extrai_links` recebe o conteúdo HTML da página como parâmetro e utiliza uma expressão regular para extrair todos os links contidos no HTML. Em seguida, os links são armazenados em um array, removendo duplicatas.

A função `main` é responsável por chamar as outras funções e realizar a execução do código principal. No exemplo, é definida uma URL de exemplo (https://www.example.com) e é feita uma requisição para essa URL. Caso a requisição seja bem-sucedida, os links são extraídos e exibidos na tela. Caso contrário, uma mensagem de erro é exibida.

Espero que este código atenda às suas necessidades! Caso precise de mais algum código ou tenha alguma dúvida, estou à disposição.