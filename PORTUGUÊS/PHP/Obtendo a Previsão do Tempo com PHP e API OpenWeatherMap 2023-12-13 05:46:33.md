Aqui está um código complexo em PHP que envolve o uso de uma API de previsão do tempo para obter informações meteorológicas e exibi-las em um formato personalizado:

```php
<?php

// Função para fazer uma requisição HTTP usando cURL
function sendRequest($url) {
    $curl = curl_init($url);
    curl_setopt($curl, CURLOPT_RETURNTRANSFER, true);
    curl_setopt($curl, CURLOPT_FOLLOWLOCATION, true);
    $response = curl_exec($curl);
    curl_close($curl);
    return $response;
}

// Função para obter a previsão do tempo para uma determinada cidade
function getWeatherForecast($cidade) {
    $apiUrl = "http://api.openweathermap.org/data/2.5/weather?q=" . urlencode($cidade) . "&appid={sua_chave_de_api_aqui}";

    $response = sendRequest($apiUrl);
    $data = json_decode($response, true);

    if ($data && isset($data['weather'][0]['main'])) {
        $condicaoAtual = $data['weather'][0]['main'];
        $temperaturaAtual = round($data['main']['temp'] - 273.15);
        $umidadeAtual = $data['main']['humidity'];
        $velocidadeVento = $data['wind']['speed'];

        return "Condição atual: $condicaoAtual\nTemperatura atual: $temperaturaAtual°C\nUmidade atual: $umidadeAtual%\nVelocidade do vento: $velocidadeVento m/s";
    } else {
        return "Não foi possível obter a previsão do tempo para $cidade";
    }
}

// Exemplo de uso
$cidade = "São Paulo";
$previsaoTempo = getWeatherForecast($cidade);
echo "Previsão do tempo para $cidade:\n";
echo $previsaoTempo;
```

Neste código, começamos definindo duas funções principais: `sendRequest`, que é responsável por fazer uma requisição HTTP usando a biblioteca cURL do PHP, e `getWeatherForecast`, que obtém a previsão do tempo para uma determinada cidade usando a API do OpenWeatherMap.

Dentro da função `getWeatherForecast`, construímos a URL da API adicionando a cidade fornecida como parâmetro e sua chave de API. Em seguida, fazemos uma requisição GET para a URL usando a função `sendRequest` e decodificamos a resposta em formato JSON.

Em seguida, verificamos se a resposta é válida e se a chave 'weather' e 'main' estão presentes nos dados. Se sim, obtemos as informações relevantes, como a condição atual, temperatura, umidade e velocidade do vento. Utilizamos a função `round` para arredondar a temperatura de Kelvin para Celsius.

Por fim, retornamos uma string formatada com as informações da previsão do tempo ou uma mensagem de erro, caso não seja possível obter a previsão para a cidade fornecida.

No exemplo de uso, definimos uma cidade ("São Paulo") e chamamos a função `getWeatherForecast` para obter a previsão do tempo. Exibimos a previsão na tela usando `echo`.