Claro! Aqui está um exemplo de código Python complexo que realiza a análise de sentimentos em um conjunto de dados de texto usando a biblioteca NLTK:

```python
import nltk
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize
from nltk.sentiment import SentimentIntensityAnalyzer

# Baixar pacotes necessários
nltk.download('punkt')
nltk.download('stopwords')
nltk.download('vader_lexicon')

def analisar_sentimentos(texto):
    # Tokenização das palavras
    tokens = word_tokenize(texto.lower())

    # Remoção de stopwords
    stopwords_pt = set(stopwords.words('portuguese'))
    tokens_filtrados = [word for word in tokens if word.isalpha() and word not in stopwords_pt]

    # Análise de sentimentos
    sia = SentimentIntensityAnalyzer()
    sentimento = sia.polarity_scores(' '.join(tokens_filtrados))

    return sentimento

def main():
    # Exemplo de texto
    texto = "Eu amo programar! A sensação de criar algo do zero é incrível."

    # Realizar análise de sentimentos
    resultado = analisar_sentimentos(texto)

    # Exibir resultado
    print("Sentimento:", resultado['compound'])
    if resultado['compound'] > 0:
        print("O texto é positivo.")
    elif resultado['compound'] < 0:
        print("O texto é negativo.")
    else:
        print("O texto é neutro.")

if __name__ == '__main__':
    main()
```

Explicação do código:
1. Importamos as bibliotecas necessárias: nltk, nltk.corpus, nltk.tokenize e nltk.sentiment.
2. Baixamos os pacotes necessários através do nltk.download.
3. Definimos a função `analisar_sentimentos` que recebe um texto como entrada. 
   - Primeiro, tokenizamos o texto em palavras utilizando `word_tokenize`.
   - Em seguida, removemos as stopwords em português utilizando `stopwords.words('portuguese')`.
   - Por fim, utilizamos o `SentimentIntensityAnalyzer` para realizar a análise de sentimentos.
4. Definimos a função `main` que é responsável por executar o código principal.
   - Nesta função, definimos um exemplo de texto.
   - Chamamos a função `analisar_sentimentos` para obter o resultado da análise.
   - Exibimos o resultado e classificamos o texto como positivo, negativo ou neutro com base no valor do compound score.
5. Por fim, verificamos se o código está sendo executado como um script principal e chamamos a função `main`.