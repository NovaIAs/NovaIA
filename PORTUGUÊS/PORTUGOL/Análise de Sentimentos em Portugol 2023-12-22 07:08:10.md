Claro! Aqui está um exemplo de código em Portugol que realiza uma análise de sentimentos em um texto fornecido pelo usuário utilizando a técnica de contagem de palavras positivas e negativas:

```
algoritmo AnaliseSentimentos

// Função para contar a quantidade de palavras positivas no texto
funcao contarPalavrasPositivas(texto: caractere[]): inteiro
    palavrasPositivas <- ["feliz", "alegre", "amor", "ótimo", "bom"] // Palavras positivas
    contador <- 0
    
    para cada palavra em texto faca
        se palavra pertence a palavrasPositivas entao
            contador <- contador + 1
        
    retorne contador

// Função para contar a quantidade de palavras negativas no texto
funcao contarPalavrasNegativas(texto: caractere[]): inteiro
    palavrasNegativas <- ["triste", "raiva", "ódio", "ruim", "chateado"] // Palavras negativas
    contador <- 0
    
    para cada palavra em texto faca
        se palavra pertence a palavrasNegativas entao
            contador <- contador + 1
        
    retorne contador

// Função para realizar a análise de sentimentos
funcao analisarSentimentos(texto: caractere[]): caractere
    palavrasPositivas <- contarPalavrasPositivas(texto)
    palavrasNegativas <- contarPalavrasNegativas(texto)
    
    se palavrasPositivas > palavrasNegativas entao
        retorne "O texto possui uma tendência positiva."
    senao se palavrasPositivas < palavrasNegativas entao
        retorne "O texto possui uma tendência negativa."
    senao
        retorne "O texto possui uma tendência neutra."

inicio
    escreva "Digite um texto para a análise de sentimentos:"
    leia texto
    
    resultado <- analisarSentimentos(texto)
    
    escreva resultado
fim
```

Neste código, temos três funções principais:
- A função `contarPalavrasPositivas` recebe um array de caracteres (texto) e retorna a quantidade de palavras positivas encontradas no texto, utilizando uma lista de palavras pré-definidas.
- A função `contarPalavrasNegativas` recebe um array de caracteres (texto) e retorna a quantidade de palavras negativas encontradas no texto, também utilizando uma lista de palavras pré-definidas.
- A função `analisarSentimentos` recebe um array de caracteres (texto) e utiliza as duas funções anteriores para contar as palavras positivas e negativas, comparando os resultados e retornando uma mensagem indicando se o texto possui uma tendência positiva, negativa ou neutra.

No programa principal, o usuário é solicitado a digitar um texto para a análise de sentimentos. O texto digitado é passado para a função `analisarSentimentos` e o resultado é exibido na tela.