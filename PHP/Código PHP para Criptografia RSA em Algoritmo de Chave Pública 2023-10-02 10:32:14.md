Claro! Aqui está um exemplo de código em PHP que cria um algoritmo de criptografia usando o algoritmo RSA:

```php
<?php

function gerarPrimo($inicio, $fim) {
    $primo = mt_rand($inicio, $fim);
    while (!ehPrimo($primo)) {
        $primo = mt_rand($inicio, $fim);
    }
    return $primo;
}

function ehPrimo($numero) {
    if ($numero <= 1) {
        return false;
    }
    if ($numero <= 3) {
        return true;
    }
    if ($numero % 2 == 0 || $numero % 3 == 0) {
        return false;
    }
    $i = 5;
    while ($i * $i <= $numero) {
        if ($numero % $i == 0 || $numero % ($i + 2) == 0) {
            return false;
        }
        $i += 6;
    }
    return true;
}

function calcularMDC($a, $b) {
    while ($b != 0) {
        $resto = $a % $b;
        $a = $b;
        $b = $resto;
    }
    return $a;
}

function calcularChavePublica($phi) {
    $e = mt_rand(2, $phi - 1);
    while (calcularMDC($e, $phi) != 1) {
        $e = mt_rand(2, $phi - 1);
    }
    return $e;
}

function calcularChavePrivada($e, $phi) {
    $d = 0;
    while ($d == 0 || $d == $e) {
        $d = mt_rand(2, $phi - 1);
    }
    return $d;
}

function criptografar($mensagem, $chave_publica, $n) {
    $tamanho_mensagem = strlen($mensagem);
    $mensagem_criptografada = "";
    for ($i = 0; $i < $tamanho_mensagem; $i++) {
        $caractere = ord($mensagem[$i]);
        $caractere_criptografado = bcpowmod($caractere, $chave_publica, $n);
        $mensagem_criptografada .= $caractere_criptografado . " ";
    }
    return trim($mensagem_criptografada);
}

function descriptografar($mensagem_criptografada, $chave_privada, $n) {
    $caracteres_criptografados = explode(" ", $mensagem_criptografada);
    $tamanho_mensagem = count($caracteres_criptografados);
    $mensagem_descriptografada = "";
    for ($i = 0; $i < $tamanho_mensagem; $i++) {
        $caractere_criptografado = intval($caracteres_criptografados[$i]);
        $caractere_descriptografado = bcpowmod($caractere_criptografado, $chave_privada, $n);
        $mensagem_descriptografada .= chr($caractere_descriptografado);
    }
    return $mensagem_descriptografada;
}

$primo1 = gerarPrimo(100, 1000);
$primo2 = gerarPrimo(100, 1000);
$n = $primo1 * $primo2;
$phi = ($primo1 - 1) * ($primo2 - 1);
$chave_publica = calcularChavePublica($phi);
$chave_privada = calcularChavePrivada($chave_publica, $phi);

$mensagem = "Olá, mundo!";
$mensagem_criptografada = criptografar($mensagem, $chave_publica, $n);
$mensagem_descriptografada = descriptografar($mensagem_criptografada, $chave_privada, $n);

echo "Mensagem original: " . $mensagem . "<br>";
echo "Mensagem criptografada: " . $mensagem_criptografada . "<br>";
echo "Mensagem descriptografada: " . $mensagem_descriptografada . "<br>";

?>
```

Neste exemplo, o código gera dois números primos aleatórios e os utiliza para calcular as chaves pública e privada do algoritmo RSA. Em seguida, uma mensagem é criptografada usando a chave pública e o valor de n (resultado da multiplicação dos dois primos). A mensagem criptografada é então descriptografada usando a chave privada e o valor de n, resultando na mensagem original.