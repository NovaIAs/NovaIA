Claro! Aqui está um código complexo em Ruby que realiza a análise de sentimentos de um texto utilizando a biblioteca `sentimental`.

```ruby
require 'sentimental'

# Carrega o analisador de sentimentos
analisador = Sentimental.new

# Define o idioma para português
analisador.load_defaults

# Função para analisar o sentimento de um texto
def analisar_sentimento(texto)
  resultado = analisador.sentiment text
  if resultado.positive?
    puts "O sentimento do texto é positivo."
  elsif resultado.negative?
    puts "O sentimento do texto é negativo."
  else
    puts "O sentimento do texto é neutro."
  end
end

# Texto de exemplo
texto = "Eu adorei o novo filme! As atuações foram incríveis e a trama me envolveu do início ao fim."

# Analisa o sentimento do texto
analisar_sentimento(texto)
```

Neste código, utilizamos a biblioteca `sentimental` para realizar a análise de sentimentos em um texto em português. 

Primeiramente, importamos a biblioteca `sentimental` e criamos uma instância do analisador de sentimentos chamada `analisador`. Em seguida, carregamos as configurações padrão para o idioma português.

A função `analisar_sentimento` recebe um texto como parâmetro e utiliza o analisador de sentimentos para determinar se o sentimento do texto é positivo, negativo ou neutro. A função imprime a resposta na tela.

No exemplo dado, a variável `texto` contém uma frase positiva sobre um filme. Chamamos a função `analisar_sentimento` passando o texto como argumento, e o código imprime "O sentimento do texto é positivo." no console.

Este código pode ser útil para empresas que desejam analisar a opinião dos clientes sobre um produto, serviço ou evento através de suas avaliações ou postagens em redes sociais, por exemplo.