Claro! Aqui está um código complexo em PHP, com um algoritmo de busca e classificação de palavras em um texto:

```php
<?php

// Função para contar as ocorrências de cada palavra em um texto
function contarPalavras($texto) {
    // Convertendo todas as letras do texto para minúsculas
    $texto = strtolower($texto);
    
    // Removendo pontuações e caracteres especiais
    $texto = preg_replace('/[^a-z0-9]+/', ' ', $texto);
    
    // Separando as palavras em um array
    $palavras = explode(' ', $texto);
    
    // Criando um array associativo para armazenar as palavras e suas contagens
    $contagens = array();
    
    // Contando as ocorrências de cada palavra
    foreach ($palavras as $palavra) {
        if (!empty($palavra)) {
            if (array_key_exists($palavra, $contagens)) {
                $contagens[$palavra]++;
            } else {
                $contagens[$palavra] = 1;
            }
        }
    }
    
    // Ordenando as palavras em ordem decrescente de contagem
    arsort($contagens);
    
    // Retornando o array de contagens
    return $contagens;
}

// Texto de exemplo
$texto = "Este é um texto de exemplo. Ele contém várias palavras repetidas, como exemplo, texto e várias. Vamos testar o algoritmo de contagem de palavras.";

// Chamando a função contarPalavras e armazenando o resultado em uma variável
$resultado = contarPalavras($texto);

// Exibindo as palavras e suas contagens
foreach ($resultado as $palavra => $contagem) {
    echo "A palavra '$palavra' ocorre $contagem vezes." . PHP_EOL;
}

?>
```

Este código PHP realiza a contagem de palavras em um texto fornecido. Ele utiliza uma função chamada `contarPalavras`, que recebe um texto como parâmetro e retorna um array associativo contendo as palavras encontradas e suas respectivas contagens.

Primeiro, o código realiza algumas operações de manipulação de strings no texto fornecido. Ele converte todas as letras para minúsculas e remove pontuações e caracteres especiais, utilizando uma expressão regular.

Em seguida, o texto é dividido em palavras, utilizando o espaço como separador. As palavras são armazenadas em um array chamado `$palavras`.

É então criado um array associativo chamado `$contagens`, para armazenar as palavras e suas contagens. A função percorre todas as palavras do array `$palavras` e verifica se a palavra já existe como chave no array `$contagens`. Se a palavra já existe, incrementa a contagem. Caso contrário, cria uma nova entrada no array com a palavra e contagem igual a 1.

Após contar todas as palavras, o array `$contagens` é ordenado em ordem decrescente de contagem, utilizando a função `arsort`.

Por fim, o código exibe as palavras e suas contagens utilizando um loop `foreach`.

Este algoritmo pode ser útil em diversas situações, como análise de textos, processamento de dados e geração de estatísticas de palavras.