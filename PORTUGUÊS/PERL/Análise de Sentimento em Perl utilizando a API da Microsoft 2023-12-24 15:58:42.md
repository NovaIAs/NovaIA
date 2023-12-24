Claro! Aqui está um código complexo em Perl que realiza a análise de sentimento de um texto fornecido pelo usuário utilizando a API de análise de sentimento da Microsoft.

```perl
#!/usr/bin/perl

use strict;
use warnings;
use LWP::UserAgent;
use JSON;

# Função para realizar a análise de sentimento
sub analisar_sentimento {
    my $texto = shift;

    my $url = 'https://westus.api.cognitive.microsoft.com/text/analytics/v3.0/sentiment';

    my $ua = LWP::UserAgent->new;
    $ua->default_header('Content-Type' => 'application/json');
    $ua->default_header('Ocp-Apim-Subscription-Key' => 'YOUR_SUBSCRIPTION_KEY');

    my $data = {
        'documents' => [
            {
                'language' => 'pt',
                'id' => '1',
                'text' => $texto
            }
        ]
    };

    my $json_data = encode_json($data);

    my $response = $ua->post($url, Content => $json_data);
    die "Erro na requisição: " . $response->status_line unless $response->is_success;

    my $result = decode_json($response->decoded_content);
    my $score = $result->{'documents'}[0]{'confidenceScores'}{'positive'};

    return $score;
}

# Solicitar o texto de entrada ao usuário
print "Digite um texto para análise de sentimento: ";
my $texto_usuario = <STDIN>;
chomp $texto_usuario;

# Realizar a análise de sentimento
my $sentimento = analisar_sentimento($texto_usuario);

# Exibir o resultado ao usuário
if ($sentimento >= 0.5) {
    print "O sentimento do texto é positivo.\n";
} else {
    print "O sentimento do texto é negativo.\n";
}
```

Neste código, começo importando o módulo `LWP::UserAgent` para realizar a requisição HTTP para a API da Microsoft. Em seguida, defino uma função `analisar_sentimento` que recebe um texto como entrada e retorna a pontuação de sentimento calculada pela API.

Dentro da função, construo a estrutura de dados necessária para enviar a requisição POST para a API, incluindo o texto fornecido pelo usuário. Em seguida, codifico os dados em formato JSON e envio a requisição.

Após receber a resposta da API, decodifico o JSON e extraio a pontuação de sentimento positivo do resultado.

No código principal, solicito ao usuário que digite um texto para análise de sentimento. Em seguida, chamo a função `analisar_sentimento` passando o texto fornecido. Com base na pontuação de sentimento retornada, imprimo uma mensagem indicando se o sentimento é positivo ou negativo.