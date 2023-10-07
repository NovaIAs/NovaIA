Claro! Aqui está um exemplo de código em PHP que realiza uma tarefa complexa: a ordenação de um array multidimensional de forma decrescente com base em um valor específico em cada subarray.

```php
<?php
// Função personalizada para ordenar o array multidimensional
function ordenarArrayMultidimensional($arr, $chave) {
    // Função de comparação para ordenar em ordem decrescente
    $comparar = function ($a, $b) use ($chave) {
        if ($a[$chave] == $b[$chave]) {
            return 0;
        }
        return ($a[$chave] > $b[$chave]) ? -1 : 1;
    };

    // Ordenar o array usando a função de comparação
    usort($arr, $comparar);

    return $arr;
}

// Array multidimensional de exemplo
$dados = [
    ['nome' => 'João', 'idade' => 25],
    ['nome' => 'Maria', 'idade' => 30],
    ['nome' => 'Pedro', 'idade' => 20]
];

// Chave pela qual o array será ordenado
$chaveOrdenar = 'idade';

// Chamar a função para ordenar o array multidimensional
$dadosOrdenados = ordenarArrayMultidimensional($dados, $chaveOrdenar);

// Exibir o resultado
foreach ($dadosOrdenados as $item) {
    echo "Nome: " . $item['nome'] . ", Idade: " . $item['idade'] . "<br>";
}
?>
```

Neste código, definimos uma função personalizada chamada `ordenarArrayMultidimensional`, que recebe um array multidimensional e uma chave como parâmetros. Essa função utiliza a função `usort` do PHP para realizar a ordenação do array com base na chave especificada.

A função `usort` recebe dois elementos do array e os compara usando uma função de comparação. No nosso caso, essa função de comparação é definida como uma função anônima (lambda) chamada `$comparar`. Essa função verifica se os valores da chave especificada são iguais e, se forem, retorna 0. Caso contrário, retorna -1 se o valor do primeiro elemento for maior que o valor do segundo elemento, ou 1 caso contrário.

Após a definição da função personalizada, criamos um array multidimensional de exemplo chamado `$dados`, contendo informações sobre pessoas (nome e idade). Em seguida, definimos a chave pela qual o array será ordenado, nesse caso, a chave 'idade'.

Por fim, chamamos a função `ordenarArrayMultidimensional` passando o array `$dados` e a chave 'idade'. O resultado da ordenação é armazenado na variável `$dadosOrdenados`. Em seguida, percorremos o array ordenado usando um loop `foreach` e exibimos os nomes e idades das pessoas na tela.

Espero que esse código atenda às suas expectativas!