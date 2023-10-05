Claro! Aqui está um código complexo em PHP que gera uma sequência de Fibonacci invertida:

```php
<?php

function fibonacci($n)
{
    if ($n <= 0) {
        return [];
    } elseif ($n == 1) {
        return [0];
    } elseif ($n == 2) {
        return [1, 0];
    } else {
        $fibonacci = [1, 0];
        for ($i = 2; $i < $n; $i++) {
            $fibonacci[$i] = $fibonacci[$i - 1] + $fibonacci[$i - 2];
        }
        return array_reverse($fibonacci);
    }
}

$numeroSequencias = 10;

echo "Sequência de Fibonacci Invertida com {$numeroSequencias} números: ";

$sequencia = fibonacci($numeroSequencias);

foreach ($sequencia as $numero) {
    echo "{$numero} ";
}

?>
```

Explicação do código:
1. Definimos uma função chamada `fibonacci` que recebe um argumento `$n`, que representa o número de elementos da sequência de Fibonacci que queremos gerar.
2. Dentro da função, verificamos se `$n` é menor ou igual a zero. Se for, retornamos um array vazio, pois não há elementos a serem gerados.
3. Em seguida, verificamos se `$n` é igual a 1. Se for, retornamos um array contendo apenas o valor 0, pois é o primeiro número da sequência.
4. Depois, verificamos se `$n` é igual a 2. Se for, retornamos um array contendo os valores 1 e 0, pois são os dois primeiros números da sequência.
5. Caso nenhum dos casos anteriores seja verdadeiro, inicializamos um array chamado `$fibonacci` com os valores 1 e 0, que correspondem aos dois primeiros números da sequência.
6. Utilizamos um loop `for` para gerar os próximos números da sequência. Começamos em 2, pois já temos os dois primeiros números definidos.
7. A cada iteração do loop, atribuímos ao elemento `$fibonacci[$i]` a soma dos dois elementos anteriores, `$fibonacci[$i - 1]` e `$fibonacci[$i - 2]`.
8. No final do loop, temos o array `$fibonacci` preenchido com a sequência de Fibonacci.
9. Utilizamos a função `array_reverse` para inverter a ordem dos elementos do array, de modo que tenhamos a sequência invertida.
10. Fora da função, definimos a variável `$numeroSequencias` com o número de elementos que queremos gerar na sequência.
11. Imprimimos uma mensagem informando a quantidade de números da sequência.
12. Chamamos a função `fibonacci` passando `$numeroSequencias` como argumento e armazenamos o resultado na variável `$sequencia`.
13. Utilizamos um loop `foreach` para percorrer os elementos de `$sequencia` e imprimi-los.
14. Ao final do código, teremos a sequência de Fibonacci invertida sendo exibida na tela.

Este código é um exemplo de como gerar a sequência de Fibonacci invertida em PHP. A complexidade aumenta à medida que o número de elementos na sequência aumenta, pois é necessário calcular e armazenar cada número até chegar ao número desejado.