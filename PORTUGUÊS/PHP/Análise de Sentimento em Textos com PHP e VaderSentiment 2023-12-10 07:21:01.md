Claro! Aqui está um código complexo em PHP que realiza uma análise de sentimento em um conjunto de textos utilizando a biblioteca VaderSentiment.

```php
<?php

// Inclui a biblioteca VaderSentiment
require_once 'VaderSentiment.php';

// Textos de exemplo
$textos = [
    'Eu amo comer pizza!',
    'O tempo está chuvoso hoje.',
    'Essa música é incrível!',
    'Estou me sentindo triste...',
    'Ele é um ótimo amigo.',
    'Essa notícia me deixou irritado.',
    'O novo filme do Batman é sensacional!'
];

// Instancia o objeto do analisador de sentimento
$vader = new VaderSentiment();

// Configuração do analisador de sentimento
$vader->setLanguage('portuguese');

// Resultados da análise de sentimento
$resultados = [];

// Realiza a análise de sentimento para cada texto
foreach ($textos as $texto) {
    $resultado = $vader->analyze($texto);
    $resultados[] = [
        'texto' => $texto,
        'sentimento' => $resultado['compound'],
        'positivo' => $resultado['pos'],
        'negativo' => $resultado['neg'],
        'neutro' => $resultado['neu']
    ];
}

// Imprime os resultados
foreach ($resultados as $resultado) {
    echo 'Texto: ' . $resultado['texto'] . PHP_EOL;
    echo 'Sentimento: ' . $resultado['sentimento'] . PHP_EOL;
    echo 'Positivo: ' . $resultado['positivo'] . PHP_EOL;
    echo 'Negativo: ' . $resultado['negativo'] . PHP_EOL;
    echo 'Neutro: ' . $resultado['neutro'] . PHP_EOL;
    echo PHP_EOL;
}

?>
```

Neste código, utilizamos a biblioteca VaderSentiment para realizar a análise de sentimento em um conjunto de textos em português. Começamos incluindo o arquivo da biblioteca `VaderSentiment.php`. Em seguida, definimos uma lista de textos de exemplo.

Após isso, instanciamos um objeto do analisador de sentimento e configuramos o idioma como "portuguese". Em seguida, iteramos sobre cada texto da lista e chamamos o método `analyze()` do objeto para realizar a análise de sentimento. Os resultados são armazenados em um array chamado `$resultados`.

Por fim, percorremos o array de resultados e imprimimos as informações de cada texto, como o texto em si, o sentimento geral (representado pelo valor `compound`), a pontuação positiva (`pos`), a pontuação negativa (`neg`) e a pontuação neutra (`neu`).

Este código é apenas um exemplo de como utilizar a biblioteca VaderSentiment para análise de sentimento em textos em português. É importante ressaltar que é necessário ter a biblioteca VaderSentiment instalada e configurada corretamente para que o código funcione adequadamente.