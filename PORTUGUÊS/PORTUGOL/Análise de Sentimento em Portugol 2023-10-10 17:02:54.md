Claro! Aqui está um código complexo em Portugol que realiza uma análise de sentimento em um texto fornecido pelo usuário.

```
algoritmo AnaliseSentimento

funcao obterTexto() : caractere
    escreva("Digite um texto: ")
    leia(texto)
    retorne texto

funcao preProcessamento(texto : caractere) : caractere
    texto <- substituirCaracteresEspeciais(texto)
    texto <- removerStopWords(texto)
    retorne texto

funcao substituirCaracteresEspeciais(texto : caractere) : caractere
    caracteresEspeciais <- {"!", "?", ".", ",", ";", ":", "(", ")", "[", "]", "{", "}", "<", ">", "'", "\"", "\n", "\t"}
    para cada caractereEspecial em caracteresEspeciais faca
        texto <- substituir(texto, caractereEspecial, " ")
    retorne texto

funcao removerStopWords(texto : caractere) : caractere
    stopwords <- {"o", "a", "os", "as", "um", "uma", "uns", "umas", "de", "do", "da", "dos", "das", "em", "no", "na", "nos", "nas", "para", "por", "com", "sem", "sobre", "entre", "pelo", "pela", "pelos", "pelas"}
    palavras <- dividir(texto, " ")
    novoTexto <- ""
    para cada palavra em palavras faca
        se palavra nao pertenceA(stopwords) entao
            novoTexto <- concatenar(novoTexto, palavra, " ")
    retorne novoTexto

funcao contarSentimentosPositivos(texto : caractere) : inteiro
    palavrasPositivas <- {"feliz", "alegre", "ótimo", "maravilhoso", "incrível"}
    palavras <- dividir(texto, " ")
    contador <- 0
    para cada palavra em palavras faca
        se palavra pertenceA(palavrasPositivas) entao
            contador <- contador + 1
    retorne contador

funcao contarSentimentosNegativos(texto : caractere) : inteiro
    palavrasNegativas <- {"triste", "chateado", "ruim", "horrível", "desanimado"}
    palavras <- dividir(texto, " ")
    contador <- 0
    para cada palavra em palavras faca
        se palavra pertenceA(palavrasNegativas) entao
            contador <- contador + 1
    retorne contador

funcao analisarSentimento(texto : caractere)
    textoPreprocessado <- preProcessamento(texto)
    quantidadePositivos <- contarSentimentosPositivos(textoPreprocessado)
    quantidadeNegativos <- contarSentimentosNegativos(textoPreprocessado)
    
    se quantidadePositivos > quantidadeNegativos entao
        escreva("O texto possui mais sentimentos positivos.")
    senao se quantidadeNegativos > quantidadePositivos entao
        escreva("O texto possui mais sentimentos negativos.")
    senao
        escreva("O texto possui uma quantidade igual de sentimentos positivos e negativos.")

inicio
    texto <- obterTexto()
    analisarSentimento(texto)
fim
```

Neste código, criamos uma função `obterTexto` que solicita ao usuário um texto para análise. Em seguida, temos a função `preProcessamento`, que realiza algumas etapas de pré-processamento no texto fornecido.

A função `substituirCaracteresEspeciais` substitui caracteres especiais, como pontuação, por espaços em branco. Isso facilita a separação das palavras posteriormente. Já a função `removerStopWords` remove as palavras que não são consideradas relevantes para a análise de sentimento, como artigos e preposições.

Em seguida, temos as funções `contarSentimentosPositivos` e `contarSentimentosNegativos`, que contam quantas palavras positivas e negativas existem no texto, respectivamente. Essas palavras são definidas nas listas `palavrasPositivas` e `palavrasNegativas`.

Por fim, a função `analisarSentimento` realiza a análise de sentimento. Ela chama as funções de pré-processamento e contagem de sentimentos, e com base nesses resultados, determina se o texto possui mais sentimentos positivos, negativos ou uma quantidade igual de ambos.

No programa principal, o texto é obtido usando a função `obterTexto` e, em seguida, passado para a função `analisarSentimento`.

Espero que este código atenda às suas expectativas!