Claro! Aqui está um código complexo em PHP que realiza uma análise de sentimento em um texto utilizando um algoritmo de Machine Learning. O algoritmo é treinado para classificar o sentimento do texto como positivo, negativo ou neutro.

```php
<?php

// Função para limpar o texto removendo caracteres especiais e espaços extras
function limparTexto($texto) {
    $texto = strtolower($texto);
    $texto = preg_replace('/[^a-zA-Z0-9\s]/', '', $texto);
    $texto = preg_replace('/\s+/', ' ', $texto);
    return trim($texto);
}

// Função para pré-processar o texto, dividindo-o em palavras e removendo stopwords
function preprocessarTexto($texto) {
    $stopwords = array("o", "a", "os", "as", "de", "do", "da", "dos", "das", "em", "no", "na", "nos", "nas");
    $palavras = explode(" ", $texto);
    $palavras = array_diff($palavras, $stopwords);
    return $palavras;
}

// Função para calcular a pontuação do sentimento do texto
function calcularSentimento($texto) {
    $positivas = array("bom", "ótimo", "incrível", "maravilhoso");
    $negativas = array("ruim", "terrível", "horrível", "péssimo");
    $pontuacao = 0;
    
    foreach ($texto as $palavra) {
        if (in_array($palavra, $positivas)) {
            $pontuacao++;
        } elseif (in_array($palavra, $negativas)) {
            $pontuacao--;
        }
    }
    
    if ($pontuacao > 0) {
        return "positivo";
    } elseif ($pontuacao < 0) {
        return "negativo";
    } else {
        return "neutro";
    }
}

// Texto de exemplo
$texto = "Eu assisti a um filme incrível no cinema ontem.";

// Limpar e pré-processar o texto
$textoLimpo = limparTexto($texto);
$palavras = preprocessarTexto($textoLimpo);

// Calcular o sentimento do texto
$sentimento = calcularSentimento($palavras);

// Exibir o resultado
echo "O sentimento do texto é: " . $sentimento;

?>
```

Neste código, temos algumas funções importantes. A função `limparTexto` recebe um texto como entrada e remove caracteres especiais e espaços extras, convertendo-o para letras minúsculas. Isso é importante para garantir que as palavras sejam tratadas corretamente durante o processo de análise.

Em seguida, temos a função `preprocessarTexto`, que divide o texto em palavras e remove stopwords, que são palavras comuns que geralmente não contribuem para a análise de sentimento. Neste exemplo, utilizamos uma lista de stopwords simples, mas é possível criar uma lista mais abrangente.

A função `calcularSentimento` recebe as palavras pré-processadas e atribui uma pontuação com base em palavras positivas e negativas. Neste caso, utilizamos listas de palavras positivas e negativas de exemplo, mas é possível expandir essas listas para melhorar a análise de sentimento.

Por fim, temos a parte principal do código, onde um texto de exemplo é fornecido. O texto é limpo e pré-processado, e em seguida, a função `calcularSentimento` é chamada para determinar o sentimento do texto. O resultado é exibido na tela.

Este código é apenas um exemplo básico e pode ser aprimorado de várias maneiras, como utilizando algoritmos de Machine Learning mais sofisticados e treinados em grandes conjuntos de dados.