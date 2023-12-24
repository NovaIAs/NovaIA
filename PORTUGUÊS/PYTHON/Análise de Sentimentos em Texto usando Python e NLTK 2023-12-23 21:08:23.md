Claro! Aqui está um código complexo em Python que realiza a análise de sentimentos usando uma abordagem baseada em processamento de linguagem natural (NLP). O código utiliza a biblioteca NLTK para tokenização, análise morfológica e classificação de sentimentos. Vou explicar o código linha por linha:

```python
import nltk
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize
from nltk.stem import WordNetLemmatizer
from nltk.sentiment import SentimentIntensityAnalyzer

nltk.download('punkt')
nltk.download('stopwords')
nltk.download('wordnet')
nltk.download('vader_lexicon')

def analisar_sentimento(texto):
    # Tokenização: dividir o texto em palavras
    palavras = word_tokenize(texto.lower())
    
    # Remoção de stopwords: eliminar palavras irrelevantes
    palavras_sem_stopwords = [palavra for palavra in palavras if palavra not in stopwords.words('portuguese')]
    
    # Lematização: redução das palavras às suas formas básicas
    lematizador = WordNetLemmatizer()
    palavras_lematizadas = [lematizador.lemmatize(palavra) for palavra in palavras_sem_stopwords]
    
    # Classificação de sentimentos usando o SentimentIntensityAnalyzer
    sid = SentimentIntensityAnalyzer()
    polaridade = sid.polarity_scores(' '.join(palavras_lematizadas))
    sentimento = 'positivo' if polaridade['compound'] >= 0 else 'negativo'
    
    return sentimento

# Exemplo de uso
texto = "Eu amo programar em Python! É a melhor linguagem de programação do mundo!"
sentimento = analisar_sentimento(texto)
print(f"Sentimento: {sentimento}")
```

Este código começa importando as bibliotecas necessárias do NLTK (Natural Language Toolkit), que é uma biblioteca popular para processamento de linguagem natural em Python. Em seguida, são realizados os downloads dos recursos linguísticos necessários do NLTK.

A função `analisar_sentimento` recebe um texto como entrada. Primeiro, o texto é tokenizado usando a função `word_tokenize`, que divide o texto em palavras individuais e as converte para letras minúsculas. Em seguida, as stopwords (palavras comuns e irrelevantes, como artigos e preposições) são removidas usando a lista de stopwords em português do NLTK.

Depois disso, as palavras restantes são lematizadas usando o lematizador WordNetLemmatizer, que reduz cada palavra à sua forma básica. Por exemplo, "amando" se torna "amar" e "programação" se torna "programa".

Finalmente, a classificação de sentimentos é realizada usando o SentimentIntensityAnalyzer do NLTK. O SentimentIntensityAnalyzer atribui uma pontuação de polaridade ao texto, que indica se o sentimento é positivo ou negativo. Se a pontuação composta (compound) for maior ou igual a 0, o sentimento é considerado positivo. Caso contrário, é considerado negativo.

No exemplo de uso fornecido, o texto "Eu amo programar em Python! É a melhor linguagem de programação do mundo!" é passado para a função `analisar_sentimento`, que retorna "positivo". Por fim, o resultado é impresso na tela.

Esse código é apenas um exemplo de como realizar a análise de sentimentos em textos usando processamento de linguagem natural. É possível adaptá-lo e ampliá-lo para atender a requisitos específicos e treinar modelos mais avançados para classificação de sentimentos.