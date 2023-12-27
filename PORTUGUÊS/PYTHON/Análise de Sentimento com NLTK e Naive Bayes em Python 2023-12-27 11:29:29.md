Claro! Aqui está um código complexo em Python que realiza uma análise de sentimento em um conjunto de textos utilizando a biblioteca NLTK e o algoritmo Naive Bayes.

```python
import nltk
from nltk.classify import NaiveBayesClassifier
from nltk.tokenize import word_tokenize
from nltk.corpus import stopwords
from nltk.stem import WordNetLemmatizer
from nltk.probability import FreqDist

# Função para pré-processar os textos
def preprocess_text(text):
    # Tokenização
    tokens = word_tokenize(text.lower())
    
    # Remoção de stopwords
    stop_words = set(stopwords.words('portuguese'))
    tokens = [token for token in tokens if token not in stop_words]
    
    # Lematização
    lemmatizer = WordNetLemmatizer()
    tokens = [lemmatizer.lemmatize(token) for token in tokens]
    
    return tokens

# Função para extrair as features dos textos
def extract_features(tokens):
    features = {}
    freq_dist = FreqDist(tokens)
    
    for token, freq in freq_dist.items():
        features[token] = freq
    
    return features

# Função para realizar a análise de sentimento
def sentiment_analysis(texts):
    # Pré-processamento dos textos
    preprocessed_texts = [preprocess_text(text) for text in texts]
    
    # Extração das features
    features = [(extract_features(tokens), 'positive') for tokens in preprocessed_texts]
    
    # Treinamento do classificador Naive Bayes
    classifier = NaiveBayesClassifier.train(features)
    
    # Classificação dos textos
    sentiment_scores = []
    for text in texts:
        tokens = preprocess_text(text)
        features = extract_features(tokens)
        sentiment_scores.append(classifier.prob_classify(features).prob('positive'))
    
    return sentiment_scores

# Exemplo de utilização da análise de sentimento
texts = [
    'Eu amei esse filme! A história é incrível e os atores são ótimos.',
    'Esse livro é muito chato. Não consegui me envolver com a história.',
    'O novo restaurante da cidade é maravilhoso. A comida é deliciosa e o atendimento é impecável.'
]

scores = sentiment_analysis(texts)

for text, score in zip(texts, scores):
    print(f'Texto: {text}')
    print(f'Sentimento: {"Positivo" if score > 0.5 else "Negativo"}')
    print(f'Probabilidade: {round(score * 100, 2)}%')
    print()
```

Neste código, utilizamos a biblioteca NLTK (Natural Language Toolkit) para realizar uma análise de sentimento em um conjunto de textos em português. A análise de sentimento é feita utilizando o algoritmo Naive Bayes, que é um classificador probabilístico simples, mas eficiente para problemas de classificação de texto.

Primeiro, definimos uma função `preprocess_text` que realiza o pré-processamento dos textos. Essa função realiza a tokenização, remoção de stopwords (palavras comuns que não contribuem para a análise de sentimento) e lematização (transformação das palavras para a sua forma original).

Em seguida, temos a função `extract_features` que extrai as features dos textos, ou seja, as palavras e suas frequências. Utilizamos a classe `FreqDist` do NLTK para contar a frequência de cada palavra nos textos.

A função principal `sentiment_analysis` recebe uma lista de textos como entrada e realiza a análise de sentimento. Ela utiliza as funções `preprocess_text` e `extract_features` para pré-processar os textos e extrair as features. Em seguida, treina um classificador Naive Bayes com as features extraídas e classifica os textos.

Por fim, temos um exemplo de utilização da análise de sentimento com três textos de exemplo. A saída do programa exibe o texto original, o sentimento (positivo ou negativo) e a probabilidade de ser um sentimento positivo (em porcentagem).