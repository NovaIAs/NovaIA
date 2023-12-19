Claro! Aqui está um código complexo em Python que realiza a análise de sentimento em um conjunto de textos usando a biblioteca NLTK (Natural Language Toolkit). O código consiste em várias etapas, incluindo pré-processamento de texto, extração de recursos e treinamento de um modelo de classificação.

```python
import nltk
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize
from nltk.stem import WordNetLemmatizer
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.model_selection import train_test_split
from sklearn.svm import SVC
from sklearn.metrics import classification_report

# Baixar recursos necessários do NLTK
nltk.download('punkt')
nltk.download('stopwords')
nltk.download('wordnet')

# Carregar o conjunto de dados
def carregar_dados():
    # Supondo que o dataset seja um dicionário com textos e rótulos
    dataset = {
        'textos': ['O filme foi excelente!', 'O livro foi muito chato.', 'Adorei a música.'],
        'rotulos': ['positivo', 'negativo', 'positivo']
    }
    return dataset

# Pré-processamento de texto
def preprocessar_texto(texto):
    # Tokenização
    tokens = word_tokenize(texto.lower())
    
    # Remoção de stopwords
    stopwords_list = set(stopwords.words('portuguese'))
    tokens = [token for token in tokens if token not in stopwords_list]
    
    # Lematização
    lemmatizer = WordNetLemmatizer()
    tokens = [lemmatizer.lemmatize(token) for token in tokens]
    
    return ' '.join(tokens)

# Extração de recursos usando TF-IDF
def extrair_recursos(textos):
    vectorizer = TfidfVectorizer()
    matriz_recursos = vectorizer.fit_transform(textos)
    return matriz_recursos

# Treinamento do modelo de classificação
def treinar_modelo(recursos, rotulos):
    X_train, X_test, y_train, y_test = train_test_split(recursos, rotulos, test_size=0.2)
    modelo = SVC()
    modelo.fit(X_train, y_train)
    return modelo

# Avaliação do modelo
def avaliar_modelo(modelo, recursos_teste, rotulos_teste):
    predicoes = modelo.predict(recursos_teste)
    relatorio_classificacao = classification_report(rotulos_teste, predicoes)
    print(relatorio_classificacao)

# Fluxo principal
def main():
    # Carregar os dados
    dataset = carregar_dados()
    
    # Pré-processamento de texto
    textos_preprocessados = [preprocessar_texto(texto) for texto in dataset['textos']]
    
    # Extração de recursos
    matriz_recursos = extrair_recursos(textos_preprocessados)
    
    # Treinamento do modelo
    modelo = treinar_modelo(matriz_recursos, dataset['rotulos'])
    
    # Avaliação do modelo
    avaliar_modelo(modelo, matriz_recursos, dataset['rotulos'])

# Executar o código principal
if __name__ == '__main__':
    main()
```

Este código começa importando as bibliotecas necessárias, como o NLTK para processamento de linguagem natural, o scikit-learn para treinamento do modelo de classificação e o WordNetLemmatizer para lematização. Em seguida, os recursos necessários do NLTK são baixados.

A função `carregar_dados()` é responsável por fornecer um conjunto de dados de exemplo contendo textos e rótulos. No exemplo, três textos são fornecidos com rótulos positivos e negativos.

A função `preprocessar_texto(texto)` realiza o pré-processamento do texto, incluindo tokenização, remoção de stopwords e lematização.

A função `extrair_recursos(textos)` usa o TF-IDF para extrair recursos dos textos pré-processados e retorna uma matriz de recursos.

A função `treinar_modelo(recursos, rotulos)` divide os dados em um conjunto de treinamento e teste, em seguida, treina um modelo de classificação SVM usando os recursos e rótulos de treinamento.

A função `avaliar_modelo(modelo, recursos_teste, rotulos_teste)` avalia o modelo treinado usando o conjunto de teste e imprime um relatório de classificação.

O fluxo principal do código é definido na função `main()`, que carrega os dados, realiza o pré-processamento, extrai recursos, treina o modelo e avalia-o.

Por fim, o código principal é executado na condição `if __name__ == '__main__':` para chamar a função `main()`.