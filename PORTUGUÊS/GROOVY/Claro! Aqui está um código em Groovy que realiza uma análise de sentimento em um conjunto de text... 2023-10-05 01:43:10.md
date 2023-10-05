Claro! Aqui está um código em Groovy que realiza uma análise de sentimento em um conjunto de textos usando a biblioteca StanfordNLP:

```groovy
@Grab(group='edu.stanford.nlp', module='stanford-corenlp', version='4.2.0')
@Grab(group='edu.stanford.nlp', module='stanford-corenlp', version='4.2.0', classifier='models')

import edu.stanford.nlp.pipeline.StanfordCoreNLP
import edu.stanford.nlp.pipeline.CoreDocument
import edu.stanford.nlp.sentiment.SentimentCoreAnnotations

// Configura o pipeline para realizar a análise de sentimento
def pipeline = new StanfordCoreNLP(props = {
    'annotators': 'tokenize,ssplit,pos,lemma,ner,sentiment'
    'tokenize.language': 'pt'
    'ssplit.newlineIsSentenceBreak': 'always'
})

// Textos de exemplo
def textos = [
    "Eu amo praia. É tão relaxante e bonita!",
    "Estou muito chateado com essa situação.",
    "Esse filme é simplesmente incrível! Recomendo para todos.",
    "Não gosto de acordar cedo. Só queria dormir mais um pouco."
]

// Processa cada texto usando o pipeline
textos.each { texto ->
    def document = new CoreDocument(texto)
    pipeline.annotate(document)

    // Extrai o sentimento do texto
    def sentimentos = document.sentences().collect {
        it.sentiment()
    }

    // Mapeia o sentimento para uma descrição
    def descricaoSentimento = sentimentos.collect {
        switch (it) {
            case 0: 'Muito negativo'
            case 1: 'Negativo'
            case 2: 'Neutro'
            case 3: 'Positivo'
            case 4: 'Muito positivo'
            default: 'Desconhecido'
        }
    }

    println "Texto: $texto"
    println "Sentimento: $descricaoSentimento"
    println ""
}
```

Neste código, utilizamos a biblioteca StanfordNLP para realizar a análise de sentimento em um conjunto de textos em português. Primeiramente, configuramos o pipeline para realizar as anotações necessárias, incluindo a análise de sentimentos.

Em seguida, definimos alguns textos de exemplo para serem processados. Cada texto é passado pelo pipeline e o sentimento é extraído utilizando o método `sentiment()`. O resultado é mapeado para uma descrição mais amigável ao usuário.

Por fim, imprimimos o texto original e o sentimento detectado para cada texto de exemplo.

Espero que esse código atenda às suas expectativas! Se precisar de mais ajuda, é só perguntar.