Certamente! Aqui está um código complexo em PHP que realiza a análise de sentimentos de um texto. 

```php
<?php

// Função para calcular o sentimento de um texto
function analisarSentimento($texto)
{
    // Dicionário de palavras positivas e seus respectivos valores
    $positivas = array("ótimo" => 5, "bom" => 4, "legal" => 3, "agradável" => 3, "feliz" => 4);
    
    // Dicionário de palavras negativas e seus respectivos valores
    $negativas = array("ruim" => -4, "triste" => -5, "horrível" => -5, "desagradável" => -3);
    
    // Vetores para armazenar as palavras do texto
    $palavras = explode(" ", $texto);
    $palavrasPositivas = array();
    $palavrasNegativas = array();
    
    // Percorre todas as palavras do texto
    foreach ($palavras as $palavra) {
        // Remove caracteres especiais e converte para minúsculas
        $palavra = strtolower(preg_replace('/[^a-zA-Z0-9À-ú ]/u', '', $palavra));
        
        // Verifica se a palavra está no dicionário de palavras positivas
        if (array_key_exists($palavra, $positivas)) {
            $palavrasPositivas[] = $palavra;
        }
        
        // Verifica se a palavra está no dicionário de palavras negativas
        if (array_key_exists($palavra, $negativas)) {
            $palavrasNegativas[] = $palavra;
        }
    }
    
    // Calcula a pontuação do sentimento
    $sentimento = array_sum(array_values($positivas)) * count($palavrasPositivas) + 
                  array_sum(array_values($negativas)) * count($palavrasNegativas);
    
    // Retorna o sentimento do texto
    return $sentimento;
}

// Exemplo de uso da função
$texto = "O filme é ótimo, adorei a história e os atores são muito bons!";
$sentimento = analisarSentimento($texto);

echo "Sentimento do texto: " . $sentimento;

?>
```

Neste código, temos uma função chamada `analisarSentimento` que recebe um texto como parâmetro. Primeiramente, definimos um dicionário de palavras positivas e outro de palavras negativas, cada uma com seus respectivos valores. Em seguida, o texto é quebrado em palavras usando a função `explode`, e as palavras são comparadas com os dicionários. 

Para cada palavra encontrada nos dicionários, ela é armazenada em um vetor correspondente (`$palavrasPositivas` ou `$palavrasNegativas`). No final, a pontuação do sentimento é calculada multiplicando a soma dos valores das palavras positivas pelo número de palavras positivas encontradas, e somando com a mesma lógica para as palavras negativas.

No exemplo de uso, o texto "O filme é ótimo, adorei a história e os atores são muito bons!" é passado para a função `analisarSentimento`. O resultado do sentimento é armazenado na variável `$sentimento` e finalmente é exibido na tela.

Esse código permite analisar o sentimento de um texto em relação a palavras positivas e negativas. Ele pode ser utilizado em aplicações de análise de sentimentos, tais como redes sociais, avaliações de produtos, entre outros. Vale ressaltar que este é apenas um exemplo básico e poderia ser aprimorado, considerando a lematização de palavras, tratamento de sinônimos, entre outras técnicas avançadas de processamento de linguagem natural.