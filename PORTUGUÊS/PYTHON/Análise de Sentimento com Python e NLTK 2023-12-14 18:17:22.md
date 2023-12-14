Claro! Abaixo está um exemplo de código complexo em Python que realiza uma análise de sentimento em um conjunto de textos. O código utiliza a biblioteca NLTK para realizar o pré-processamento dos dados, além de utilizar um modelo de aprendizado de máquina para fazer a classificação dos textos em sentimentos positivos ou negativos.

```python
import nltk
import random
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize
from nltk.stem import PorterStemmer
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.model_selection import train_test_split
from sklearn.svm import SVC

# Baixar recursos necessários do NLTK
nltk.download('stopwords')
nltk.download('punkt')

# Carregar dados de treinamento
def load_data():
    positive_reviews = open('positive_reviews.txt', 'r', encoding='utf-8').readlines()
    negative_reviews = open('negative_reviews.txt', 'r', encoding='utf-8').readlines()
    return positive_reviews, negative_reviews

# Pré-processamento dos dados
def preprocess_data(positive_reviews, negative_reviews):
    reviews = positive_reviews + negative_reviews
    labels = ['positive'] * len(positive_reviews) + ['negative'] * len(negative_reviews)
    
    # Remover stopwords, pontuação e realizar stemming dos tokens
    stop_words = set(stopwords.words('portuguese'))
    stemmer = PorterStemmer()
    
    preprocessed_reviews = []
    for review in reviews:
        tokens = word_tokenize(review.lower())  # Tokenização e conversão para lowercase
        filtered_tokens = [stemmer.stem(token) for token in tokens if token.isalpha() and token not in stop_words]  # Remoção de stopwords, pontuação e stemming
        preprocessed_reviews.append(' '.join(filtered_tokens))
    
    return preprocessed_reviews, labels

# Treinar modelo de análise de sentimento
def train_model(reviews, labels):
    vectorizer = TfidfVectorizer()  # Vetorização das palavras
    X = vectorizer.fit_transform(reviews)
    y = labels
    
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)
    
    model = SVC(kernel='linear')  # Classificador SVM
    model.fit(X_train, y_train)
    
    return model, vectorizer, X_test, y_test

# Avaliar modelo
def evaluate_model(model, vectorizer, X_test, y_test):
    X_test_transformed = vectorizer.transform(X_test)
    accuracy = model.score(X_test_transformed, y_test)
    return accuracy

# Realizar análise de sentimento
def analyze_sentiment(model, vectorizer, text):
    preprocessed_text = preprocess_data([text], [])[0]
    text_transformed = vectorizer.transform(preprocessed_text)
    sentiment = model.predict(text_transformed)[0]
    return sentiment

# Execução do programa
if __name__ == '__main__':
    positive_reviews, negative_reviews = load_data()
    preprocessed_reviews, labels = preprocess_data(positive_reviews, negative_reviews)
    model, vectorizer, X_test, y_test = train_model(preprocessed_reviews, labels)
    accuracy = evaluate_model(model, vectorizer, X_test, y_test)
    
    # Exemplo de análise de sentimento
    text = "O filme foi muito bom, gostei bastante!"
    sentiment = analyze_sentiment(model, vectorizer, text)
    
    print("Acurácia do modelo: ", accuracy)
    print("Sentimento do texto: ", sentiment)
```

Neste código, há várias etapas envolvidas:

1. Importação das bibliotecas necessárias, como `nltk`, `random`, `sklearn`.
2. Download dos recursos necessários do NLTK, como stopwords e tokenizadores.
3. Carregamento dos dados de treinamento de arquivos `positive_reviews.txt` e `negative_reviews.txt`.
4. Pré-processamento dos dados, incluindo remoção de stopwords, pontuação e stemming dos tokens.
5. Vetorização das palavras utilizando `TfidfVectorizer`.
6. Divisão dos dados em conjuntos de treinamento e teste.
7. Treinamento do modelo de análise de sentimento utilizando SVM.
8. Avaliação do modelo utilizando a acurácia.
9. Exemplo de análise de sentimento de um texto.
10. Impressão dos resultados.

Este código implementa um sistema de análise de sentimento básico que pode ser utilizado para classificar textos em sentimentos positivos ou negativos. É importante notar que este é apenas um exemplo e, dependendo do seu conjunto de dados, podem ser necessárias adaptações e ajustes no código.