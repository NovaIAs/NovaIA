Claro! Aqui está um exemplo de um código complexo em Ruby que realiza a análise de sentimento de um texto usando a biblioteca "sentimental_pt":

```ruby
require 'sentimental_pt'

def analisar_sentimento(texto)
  analisador = SentimentalPT::Analisador.new
  resultado = analisador.sentimento(texto)
  
  case resultado
  when :positivo
    puts "O sentimento do texto é positivo."
  when :negativo
    puts "O sentimento do texto é negativo."
  when :neutro
    puts "O sentimento do texto é neutro."
  else
    puts "Não foi possível determinar o sentimento do texto."
  end
end

puts "Digite um texto para análise de sentimento:"
texto = gets.chomp

analisar_sentimento(texto)
```

Neste código, começamos importando a biblioteca "sentimental_pt" que é responsável por realizar a análise de sentimento em textos em português. Em seguida, definimos a função "analisar_sentimento" que recebe um texto como parâmetro.

Dentro da função, criamos uma instância do analisador de sentimentos e chamamos o método "sentimento" passando o texto como argumento. O método retorna um símbolo que representa o sentimento do texto: ":positivo" para sentimento positivo, ":negativo" para sentimento negativo, ":neutro" para sentimento neutro e outros valores para quando não é possível determinar o sentimento.

Em seguida, utilizamos uma estrutura de caso (case) para imprimir uma mensagem correspondente ao sentimento identificado. Por fim, pedimos ao usuário para digitar um texto, chamamos a função "analisar_sentimento" passando o texto digitado e exibimos o resultado.

Espero que este código atenda às suas expectativas!