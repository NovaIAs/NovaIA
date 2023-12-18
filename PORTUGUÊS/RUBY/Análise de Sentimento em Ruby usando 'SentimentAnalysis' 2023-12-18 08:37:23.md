Certamente! Aqui está um código em Ruby que realiza uma análise de sentimento em um conjunto de textos, utilizando a biblioteca 'SentimentAnalysis'. O código irá calcular a polaridade de cada texto (positivo, negativo ou neutro) e exibirá o resultado.

```
require 'sentiment_analysis'

# Função que realiza a análise de sentimentos
def analisar_sentimento(texto)
  # Inicializa o objeto de análise de sentimentos
  sentiment_analysis = SentimentAnalysis.new

  # Realiza a análise de sentimento do texto fornecido
  resultado = sentiment_analysis.sentiment(texto)

  # Exibe o resultado
  case resultado
  when 'positive'
    puts "O texto '#{texto}' possui polaridade positiva."
  when 'negative'
    puts "O texto '#{texto}' possui polaridade negativa."
  else
    puts "O texto '#{texto}' possui polaridade neutra."
  end
end

# Textos a serem analisados
textos = [
  "Eu amo o meu trabalho, é muito gratificante.",
  "Estou extremamente desapontado com o resultado.",
  "Hoje está um dia muito bonito.",
  "A comida estava deliciosa, recomendo!",
  "Não gostei do atendimento no restaurante.",
  "Estou me sentindo neutro em relação a isso."
]

# Realiza a análise de sentimento para cada texto
textos.each do |texto|
  analisar_sentimento(texto)
end
```

Neste código, é necessário instalar a gem 'sentiment_analysis' antes de executá-lo, utilizando o comando `gem install sentiment_analysis`. 

O código começa importando a biblioteca 'sentiment_analysis'. Em seguida, a função `analisar_sentimento` é definida para realizar a análise de sentimentos. Dentro dessa função, um objeto `sentiment_analysis` é inicializado e, em seguida, a função `sentiment` é chamada para calcular a polaridade do texto fornecido.

O resultado é então exibido utilizando uma estrutura de controle `case`. Se o resultado for 'positive', é exibida uma mensagem indicando que o texto possui polaridade positiva. Se o resultado for 'negative', é exibida uma mensagem indicando polaridade negativa. Caso contrário, é exibida uma mensagem indicando polaridade neutra.

No final do código, uma array `textos` é definida com alguns exemplos de textos a serem analisados. O laço `each` percorre cada texto na array e chama a função `analisar_sentimento` para realizar a análise de sentimento. Os resultados são exibidos no console.