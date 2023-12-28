Claro! Aqui está um exemplo de código em Groovy que realiza uma análise de sentimentos de um texto usando a API do IBM Watson Natural Language Understanding:

```groovy
@Grab('com.ibm.watson.developer_cloud:natural-language-understanding:6.10.0')

import com.ibm.watson.developer_cloud.natural_language_understanding.v1.NaturalLanguageUnderstanding
import com.ibm.watson.developer_cloud.natural_language_understanding.v1.model.AnalysisResults
import com.ibm.watson.developer_cloud.natural_language_understanding.v1.model.SentimentOptions
import com.ibm.watson.developer_cloud.natural_language_understanding.v1.model.AnalyzeOptions

def texto = "Eu amo usar a linguagem Groovy! É muito poderosa e eficiente."

def apiKey = "SUA_API_KEY_DO_IBM_WATSON"
def apiUrl = "https://gateway.watsonplatform.net/natural-language-understanding/api"

def nlu = new NaturalLanguageUnderstanding()
nlu.setApiKey(apiKey)
nlu.setEndPoint(apiUrl)

def sentimentOptions = new SentimentOptions.Builder().build()
def analyzeOptions = new AnalyzeOptions.Builder().text(texto).features(sentimentOptions).build()

AnalysisResults results = nlu.analyze(analyzeOptions).execute()

def sentiment = results.sentiment.document.label
def score = results.sentiment.document.score

println "O sentimento do texto é ${sentiment} com uma pontuação de ${score}"
```

Neste exemplo, utilizamos a biblioteca `com.ibm.watson.developer_cloud:natural-language-understanding` para acessar a API do IBM Watson Natural Language Understanding. Primeiro, é necessário importar as classes necessárias para a análise de sentimentos.

Em seguida, definimos um texto de exemplo que queremos analisar. Em seguida, definimos a chave de API (`apiKey`) e a URL da API (`apiUrl`) do IBM Watson Natural Language Understanding.

Criamos uma instância do `NaturalLanguageUnderstanding` e definimos a chave de API e a URL da API.

Em seguida, criamos as opções de análise de sentimentos (`SentimentOptions`) e as opções de análise (`AnalyzeOptions`) com o texto e as opções de sentimentos.

Por fim, executamos a análise chamando o método `analyze` da instância do `NaturalLanguageUnderstanding` e armazenamos o resultado na variável `results`. Extraímos o rótulo do sentimento e a pontuação do documento e as exibimos no console.

Este código é apenas um exemplo de como usar a API do IBM Watson Natural Language Understanding para realizar a análise de sentimentos. Lembre-se de substituir `SUA_API_KEY_DO_IBM_WATSON` pela sua chave de API válida.