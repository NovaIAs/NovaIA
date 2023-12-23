Aqui está um exemplo de um código complexo em PHP, que utiliza várias funcionalidades e é explicado linha por linha:

```php
<?php

// Função para verificar se um número é primo
function ehPrimo($numero)
{
    if ($numero <= 1) {
        return false;
    }

    for ($i = 2; $i <= sqrt($numero); $i++) {
        if ($numero % $i == 0) {
            return false;
        }
    }

    return true;
}

// Função para calcular o fatorial de um número
function fatorial($numero)
{
    if ($numero <= 1) {
        return 1;
    }

    return $numero * fatorial($numero - 1);
}

// Função para inverter uma string
function inverterString($string)
{
    return strrev($string);
}

// Função para calcular a soma dos dígitos de um número
function somaDigitos($numero)
{
    $soma = 0;

    while ($numero > 0) {
        $soma += $numero % 10;
        $numero = (int)($numero / 10);
    }

    return $soma;
}

// Verifica se um número é primo
$numero = 13;
if (ehPrimo($numero)) {
    echo "{$numero} é primo." . PHP_EOL;
} else {
    echo "{$numero} não é primo." . PHP_EOL;
}

// Calcula o fatorial de um número
$numero = 5;
$resultado = fatorial($numero);
echo "O fatorial de {$numero} é {$resultado}." . PHP_EOL;

// Inverte uma string
$string = "Olá, mundo!";
$resultado = inverterString($string);
echo "A string invertida é: {$resultado}" . PHP_EOL;

// Calcula a soma dos dígitos de um número
$numero = 12345;
$resultado = somaDigitos($numero);
echo "A soma dos dígitos de {$numero} é {$resultado}." . PHP_EOL;

?>
```

Neste código em PHP, temos algumas funcionalidades interessantes:

1. A função `ehPrimo` verifica se um número é primo, utilizando um algoritmo básico de divisão. Ela retorna `true` se o número for primo e `false` caso contrário.

2. A função `fatorial` calcula o fatorial de um número, utilizando recursão. Ela retorna o valor do fatorial.

3. A função `inverterString` inverte uma string utilizando a função `strrev` do PHP. Ela retorna a string invertida.

4. A função `somaDigitos` calcula a soma dos dígitos de um número utilizando um laço `while`. Ela retorna a soma dos dígitos.

Em seguida, o código chama cada uma dessas funções para realizar algumas operações:

1. Verifica se o número 13 é primo e exibe uma mensagem correspondente.

2. Calcula o fatorial do número 5 e exibe o resultado.

3. Inverte a string "Olá, mundo!" e exibe o resultado.

4. Calcula a soma dos dígitos do número 12345 e exibe o resultado.

Esse código é apenas um exemplo de como podemos utilizar diferentes funcionalidades do PHP para realizar diferentes tarefas. Ele pode ser adaptado e modificado de acordo com as necessidades específicas de cada projeto.