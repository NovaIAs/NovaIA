Claro! Abaixo está um código em Ruby que realiza uma análise de sentimento em um texto utilizando o algoritmo Naive Bayes. O algoritmo treina um modelo com base em um conjunto de dados classificados e, em seguida, classifica novos textos como positivos ou negativos.

```ruby
require 'csv'
require 'set'

class SentimentAnalysis
  def initialize
    @word_counts = {}
    @class_counts = { positive: 0, negative: 0 }
    @vocabulary = Set.new
  end

  def train(file_path)
    CSV.foreach(file_path, col_sep: ';') do |row|
      text = row[0]
      sentiment = row[1].to_sym
      train_text(text, sentiment)
    end
  end

  def classify(text)
    positive_prob = calculate_class_probability(text, :positive)
    negative_prob = calculate_class_probability(text, :negative)

    if positive_prob > negative_prob
      "Positivo"
    else
      "Negativo"
    end
  end

  private

  def train_text(text, sentiment)
    words = text.downcase.split

    words.each do |word|
      @word_counts[word] ||= { positive: 0, negative: 0 }
      @class_counts[sentiment] += 1
      @vocabulary.add(word)

      @word_counts[word][sentiment] += 1
    end
  end

  def calculate_class_probability(text, sentiment)
    words = text.downcase.split
    prob = @class_counts[sentiment].to_f / (@class_counts[:positive] + @class_counts[:negative])

    words.each do |word|
      if @vocabulary.include?(word)
        prob *= (@word_counts[word][sentiment].to_f + 1) / (@class_counts[sentiment] + @vocabulary.size)
      end
    end

    prob
  end
end

# Exemplo de uso:
analisador = SentimentAnalysis.new
analisador.train('dados_treino.csv') # Arquivo CSV com textos e suas classificações (positivo ou negativo)

texto = "Eu adorei esse filme, foi incrível!"
resultado = analisador.classify(texto)
puts "O sentimento do texto é: #{resultado}"
```

Neste código, utilizamos a biblioteca `CSV` para ler um arquivo CSV contendo textos e suas classificações (positivo ou negativo). A classe `SentimentAnalysis` possui métodos para treinar o modelo (`train`) e classificar novos textos (`classify`).

No método `train`, percorremos cada linha do arquivo CSV e realizamos o treinamento do modelo. Para cada texto, dividimos as palavras em minúsculas e atualizamos as contagens de palavras e classes.

No método `classify`, calculamos a probabilidade do texto pertencer a cada classe (positivo e negativo) usando o algoritmo Naive Bayes. No final, comparamos as probabilidades e retornamos a classificação final.

Este código é apenas um exemplo simples de análise de sentimento em Ruby utilizando o algoritmo Naive Bayes. Dependendo dos requisitos e do contexto do problema, outras técnicas e bibliotecas podem ser mais adequadas.