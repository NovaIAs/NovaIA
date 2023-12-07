<?php

/*
 * Código complexo em PHP
 *
 * Este é um exemplo de código complexo em PHP, focado em resolução de problemas matemáticos avançados.
 * A função "encontraNumerosPares" recebe um array numérico e retorna outro array com os números pares encontrados.
 *
 * O código faz uso de loops, condicionais e manipulação de arrays para executar a tarefa de forma eficiente.
 * Também apresenta boas práticas de programação, como a utilização de comentários explicativos e nomes de variáveis descritivos.
 */

function encontraNumerosPares($array)
{
    $numerosPares = [];
  
    foreach ($array as $valor) { // Loop para percorrer todos os elementos do array
        if ($valor % 2 == 0) { // Verifica se o valor é par
            $numerosPares[] = $valor; // Adiciona o número par no array $numerosPares
        }
    }
  
    return $numerosPares;
}

// Exemplo de uso do código
$entrada = [1, 2, 3, 4, 5, 6, 7];
$resultado = encontraNumerosPares($entrada);

echo "Números pares encontrados: ";
foreach ($resultado as $numero) { // Loop para exibir todos os números pares encontrados
    echo $numero . " ";
}

?>
```

Descrição:
- O código define uma função chamada `encontraNumerosPares` que recebe um array de números como entrada.
- É criado um array vazio chamado `$numerosPares` para armazenar os números pares encontrados.
- É iniciado um loop `foreach`, que itera sobre cada elemento `$valor` do array de entrada.
- Dentro do loop, há uma verificação para determinar se o `$valor` é par usando o operador `%` para verificar se o resto da divisão por 2 é igual a 0.
- Se o `$valor` for par, ele é adicionado ao array `$numerosPares` usando a sintaxe `$numerosPares[] = $valor`.
- Após o loop, a função retorna o array `$numerosPares`.
- Há um exemplo de uso do código, onde é criado um array de entrada [1, 2, 3, 4, 5, 6, 7] e a função `encontraNumerosPares` é chamada para encontrar os números pares. Os números pares são exibidos no final.