Claro! Aqui está um código complexo em PHP, que gera uma lista de números primos usando o Crivo de Eratóstenes:

```php
<?php

function encontrarPrimos($limite) {
    $numeros = array_fill(2, $limite-1, true);

    for ($i = 2; $i <= sqrt($limite); $i++) {
        if ($numeros[$i]) {
            for ($j = $i ** 2; $j <= $limite; $j += $i) {
                $numeros[$j] = false;
            }
        }
    }

    $primos = [];

    foreach ($numeros as $numero => $ehPrimo) {
        if ($ehPrimo) {
            $primos[] = $numero;
        }
    }

    return $primos;
}

$limite = 100;
$primosEncontrados = encontrarPrimos($limite);

echo "Os números primos até $limite são: \n";
echo implode(", ", $primosEncontrados);

?>
```

Neste código, a função `encontrarPrimos` recebe um limite como parâmetro e retorna um array contendo todos os números primos encontrados até esse limite. 

Primeiro, criamos um array chamado `$numeros`, preenchendo-o com valores booleanos `true` para todos os números a partir de 2 até o limite informado. Utilizamos a função `array_fill` para isso.

Em seguida, percorremos todos os números de 2 até a raiz quadrada do limite, utilizando o laço `for`. Dentro desse laço, verificamos se o número atual é um número primo. Se for, marcamos todos os múltiplos desse número como não primos, percorrendo o array `$numeros` com um novo laço `for`. Isso é feito atribuindo `false` às posições do array correspondentes aos múltiplos.

Depois de percorrer todos os números até a raiz quadrada do limite, criamos um novo array chamado `$primos` e adicionamos nele todos os números que ainda possuem o valor `true` no array `$numeros`.

Por fim, imprimimos a lista de números primos encontrados até o limite informado, utilizando a função `implode` para unir os elementos do array em uma string, separados por vírgula, e a função `echo` para exibir a mensagem na tela.

Esse código utiliza o Crivo de Eratóstenes, um algoritmo muito eficiente para encontrar números primos. Espero que essa implementação atenda às suas expectativas!