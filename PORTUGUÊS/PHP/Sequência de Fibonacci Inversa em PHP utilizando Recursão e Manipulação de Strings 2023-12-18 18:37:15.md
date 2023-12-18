Claro! Aqui está um código complexo em PHP que gera uma sequência de Fibonacci inversa, utilizando recursão e manipulação de strings:

```php
<?php

function fibonacciInverso($n) {
    if ($n <= 0) {
        return "";
    } elseif ($n == 1) {
        return "1";
    } else {
        $fibonacci = fibonacciInverso($n - 1);
        $length = strlen($fibonacci);
        if ($length >= 2) {
            $lastChar1 = substr($fibonacci, -1);
            $lastChar2 = substr($fibonacci, -2, 1);
            if ($lastChar1 == "0") {
                for ($i = $length - 2; $i >= 0; $i--) {
                    if ($fibonacci[$i] != "0") {
                        $fibonacci[$i] = strval(intval($fibonacci[$i]) - 1);
                        break;
                    }
                }
                $fibonacci[$length - 1] = "9";
                $fibonacci .= "8";
            } elseif ($lastChar2 == "0") {
                $fibonacci[$length - 2] = strval(intval($fibonacci[$length - 2]) - 1);
                $fibonacci[$length - 1] = "9";
            } else {
                $lastChar1 = substr($fibonacci, -1);
                $lastChar2 = substr($fibonacci, -2, 1);
                $fibonacci = substr($fibonacci, 0, $length - 2) . strval(intval($lastChar1) - 1) . "9" . $lastChar2;
            }
        } else {
            $fibonacci = strval(intval($fibonacci) - 1);
            $fibonacci .= "9";
        }
        return $fibonacci;
    }
}

$numero = 10;
echo "Sequência de Fibonacci inversa dos primeiros $numero números: \n";

for ($i = 1; $i <= $numero; $i++) {
    echo fibonacciInverso($i) . "\n";
}
```

Este código em PHP gera, de forma recursiva, a sequência de Fibonacci inversa dos primeiros N números. A função `fibonacciInverso` recebe como argumento um número inteiro N e retorna uma string contendo a sequência de Fibonacci inversa até o número N.

O código utiliza a recursão para calcular cada termo da sequência de Fibonacci inversa, começando a partir do número 1. Para evitar o uso de números inteiros negativos, são feitas manipulações de strings para simular a subtração dos números na sequência.

Em cada iteração da função, são verificados os dois últimos dígitos da sequência gerada até o momento. Se o último dígito for zero, é necessário encontrar o próximo número não-zero na sequência e diminuí-lo em uma unidade. Caso o penúltimo dígito seja zero, apenas é necessário diminuí-lo em uma unidade. Caso contrário, subtrai-se uma unidade do último dígito e coloca-se os dígitos atualizados na sequência.

No final, o código imprime a sequência de Fibonacci inversa dos primeiros N números.