Claro! Aqui está um código complexo em PHP que realiza a geração de um número pseudoaleatório baseado em um algoritmo de criptografia simétrica:

```php
<?php
/**
 * Função para gerar um número pseudoaleatório
 * baseado em um algoritmo de criptografia simétrica.
 *
 * @param int $seed Semente para geração do número.
 * @param int $min Valor mínimo do número gerado.
 * @param int $max Valor máximo do número gerado.
 *
 * @return int Número pseudoaleatório gerado.
 */
function geraNumeroPseudoaleatorio($seed, $min, $max)
{
    $chave = md5($seed); // Gera uma chave de criptografia baseada na semente

    // Cria um vetor de inicialização (IV) com base na chave
    $iv = substr($chave, 0, 16);

    // Gera uma chave de criptografia com base na chave e no IV
    $chaveCriptografia = substr($chave, 16, 16);

    // Define a quantidade de bytes a serem gerados
    $quantidadeBytes = ceil(log($max - $min, 2) / 8);

    // Inicializa o vetor de bytes
    $bytes = '';

    // Gera os bytes pseudoaleatórios
    while (strlen($bytes) < $quantidadeBytes) {
        $bytes .= openssl_encrypt($chaveCriptografia, 'AES-128-CBC', $chaveCriptografia, OPENSSL_RAW_DATA, $iv);
    }

    // Converte os bytes para um número inteiro dentro do intervalo desejado
    $numero = hexdec(bin2hex($bytes));
    $numero = $numero % ($max - $min + 1);
    $numero += $min;

    return $numero;
}

// Exemplo de uso da função
$numero = geraNumeroPseudoaleatorio(123456, 1, 100);
echo "Número gerado: " . $numero;
?>
```

Neste código, temos uma função `geraNumeroPseudoaleatorio` que recebe três parâmetros: a semente para a geração do número, o valor mínimo e o valor máximo do intervalo no qual o número deve ser gerado.

A função utiliza o algoritmo de criptografia simétrica AES-128-CBC para gerar um número pseudoaleatório. Primeiro, é gerada uma chave de criptografia a partir da semente utilizando o algoritmo MD5. Em seguida, um vetor de inicialização (IV) é criado a partir da chave.

Depois, a função determina a quantidade de bytes necessários para representar todos os números dentro do intervalo desejado. Utilizando o OpenSSL, ela gera os bytes pseudoaleatórios concatenando-os até atingir a quantidade necessária.

Por fim, os bytes são convertidos em um número inteiro dentro do intervalo desejado, garantindo uma distribuição uniforme dos números gerados.

No exemplo de uso apresentado, a função é chamada com a semente 123456 e os valores mínimo e máximo definidos como 1 e 100, respectivamente. O número gerado é então exibido na tela.

Vale ressaltar que, embora este código seja complexo, é importante lembrar que a geração de números verdadeiramente aleatórios é impossível em computadores determinísticos. No entanto, o algoritmo utilizado neste código é capaz de gerar números pseudoaleatórios satisfatoriamente para a maioria das aplicações.