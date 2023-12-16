Claro! Aqui está um código complexo em Python que realiza uma análise de sentimento em texto usando a biblioteca NLTK (Natural Language Toolkit). Ele é um pouco extenso, mas espero que seja útil para você:

```python
import nltk
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize
from nltk.sentiment import SentimentIntensityAnalyzer

def analisar_sentimento(texto):
    # Tokenização do texto
    palavras = word_tokenize(texto, language='portuguese')
    
    # Remoção de stopwords
    stop_words = set(stopwords.words('portuguese'))
    palavras_filtradas = [palavra.lower() for palavra in palavras if palavra.lower() not in stop_words]
    
    # Análise de sentimento
    sia = SentimentIntensityAnalyzer()
    resultado = sia.polarity_scores(' '.join(palavras_filtradas))
    
    # Determinar o sentimento do texto
    if resultado['compound'] >= 0.05:
        sentimento = 'Positivo'
    elif resultado['compound'] <= -0.05:
        sentimento = 'Negativo'
    else:
        sentimento = 'Neutro'
    
    return sentimento

# Exemplo de uso
texto = "Eu adorei o filme! A atuação dos atores foi incrível."
sentimento = analisar_sentimento(texto)
print(f"Sentimento: {sentimento}")
```

Neste código, utilizamos a biblioteca NLTK para realizar a análise de sentimento em um texto em português. 

Primeiro, importamos as dependências necessárias: `nltk`, `stopwords`, `word_tokenize` e `SentimentIntensityAnalyzer` da biblioteca NLTK. 

Em seguida, definimos a função `analisar_sentimento`, que recebe um texto como argumento. 

Dentro da função, realizamos a tokenização do texto usando a função `word_tokenize`, passando o texto e especificando a linguagem como 'portuguese'. Em seguida, usamos a função `set(stopwords.words('portuguese'))` para obter um conjunto de stopwords em português.

Filtramos as palavras do texto, convertendo-as para minúsculas e verificando se estão presentes no conjunto de stopwords. 

Em seguida, utilizamos a classe `SentimentIntensityAnalyzer` para calcular a polaridade do texto filtrado. A função `polarity_scores` retorna um dicionário com os valores de polaridade, incluindo o valor composto (`compound`), que é uma pontuação geral do sentimento.

Com base na pontuação composta, definimos o sentimento do texto como "Positivo" se a pontuação for maior ou igual a 0.05, como "Negativo" se for menor ou igual a -0.05, ou como "Neutro" caso contrário.

Por fim, executamos um exemplo de uso, fornecendo um texto de exemplo e imprimindo o sentimento analisado. 

Espero que este código atenda às suas expectativas!