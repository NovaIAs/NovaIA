Claro! Aqui está um exemplo de um código complexo em PHP, que utiliza uma estrutura de controle e manipulação de arrays:

```
<?php
// Função para verificar se um número é primo
function isPrimo($num)
{
    if ($num <= 1) {
        return false;
    }

    for ($i = 2; $i <= sqrt($num); $i++) {
        if ($num % $i == 0) {
            return false;
        }
    }

    return true;
}

// Função para gerar um array com números primos
function gerarArrayPrimos($qtde)
{
    $primos = [];
    $num = 2;

    while (count($primos) < $qtde) {
        if (isPrimo($num)) {
            $primos[] = $num;
        }
        $num++;
    }

    return $primos;
}

// Função para imprimir o array na tela
function imprimirArray($array)
{
    foreach ($array as $valor) {
        echo $valor . " ";
    }
    echo "\n";
}

// Definindo a quantidade de números primos a serem gerados
$qtdePrimos = 10;

// Gerando o array de números primos
$arrayPrimos = gerarArrayPrimos($qtdePrimos);

// Imprimindo o array na tela
imprimirArray($arrayPrimos);
?>
```

Neste código, temos três funções definidas:

1. A função `isPrimo($num)` verifica se um número é primo ou não, utilizando um loop `for` e a função `sqrt` para calcular a raiz quadrada do número e determinar o limite do loop.

2. A função `gerarArrayPrimos($qtde)` gera um array com a quantidade especificada de números primos. Ela utiliza a função `isPrimo` para verificar se cada número gerado é primo, e caso seja, adiciona-o ao array.

3. A função `imprimirArray($array)` percorre o array e imprime cada elemento na tela, separados por um espaço.

No código principal, é definida a quantidade de números primos a serem gerados (`$qtdePrimos`) e, em seguida, é chamada a função `gerarArrayPrimos` para gerar o array de números primos. Por fim, o array é impresso na tela utilizando a função `imprimirArray`.

Espero que este código atenda às suas expectativas!