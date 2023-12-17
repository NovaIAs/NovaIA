Claro! Aqui está um código em Python que faz uma análise de sentimento de um texto utilizando machine learning e a biblioteca NLTK (Natural Language Toolkit):

```python
import nltk
from nltk.sentiment import SentimentIntensityAnalyzer

# Função para realizar a análise de sentimento
def analisar_sentimento(texto):
    # Inicializa o SentimentIntensityAnalyzer
    sia = SentimentIntensityAnalyzer()
    
    # Realiza a análise de sentimento do texto
    pontuacao = sia.polarity_scores(texto)
    
    # Retorna o resultado da análise de sentimento
    if pontuacao['compound'] >= 0.05:
        return 'positivo'
    elif pontuacao['compound'] <= -0.05:
        return 'negativo'
    else:
        return 'neutro'

# Texto de exemplo para análise de sentimento
texto = "Eu amo programar! É incrível ver como as coisas funcionam."

# Realiza a análise de sentimento do texto
sentimento = analisar_sentimento(texto)

# Imprime o resultado da análise de sentimento
print(f"O sentimento do texto é {sentimento}.")
```

Neste código, primeiro importamos a biblioteca `nltk` e a classe `SentimentIntensityAnalyzer` da sub-biblioteca `sentiment`. Em seguida, definimos a função `analisar_sentimento`, que recebe um texto como parâmetro.

Dentro da função, criamos uma instância do `SentimentIntensityAnalyzer` e utilizamos o método `polarity_scores` para realizar a análise de sentimento do texto. Esse método retorna uma pontuação que representa o sentimento do texto, onde valores positivos indicam sentimento positivo, valores negativos indicam sentimento negativo e valores próximos a zero indicam sentimento neutro.

A partir da pontuação obtida, comparamos o valor do atributo `compound` com um limiar para determinar se o sentimento é positivo, negativo ou neutro. Caso o valor seja maior ou igual a 0.05, consideramos como positivo. Se for menor ou igual a -0.05, consideramos como negativo. Caso contrário, consideramos como neutro.

Por fim, definimos um texto de exemplo e chamamos a função `analisar_sentimento` passando esse texto como argumento. O resultado é armazenado na variável `sentimento` e é exibido na tela.

Esse código utiliza técnicas de processamento de linguagem natural (NLP) para realizar a análise de sentimento de um texto. É possível adaptá-lo para realizar análises mais complexas, utilizando outros modelos de machine learning ou incorporando outras funcionalidades.