Claro! Aqui está um código complexo em PHP que realiza a busca de um elemento específico em uma matriz multidimensional:

```php
<?php
// Função para buscar um elemento em uma matriz multidimensional
function buscarElementoMatriz($matriz, $elemento) {
    foreach ($matriz as $valor) {
        if (is_array($valor)) {
            $resultado = buscarElementoMatriz($valor, $elemento);
            if ($resultado !== false) {
                return $resultado;
            }
        } else {
            if ($valor === $elemento) {
                return true;
            }
        }
    }
    return false;
}

// Matriz multidimensional de exemplo
$matriz = array(
    array(1, 2, 3),
    array(4, array(5, 6)),
    array(array(7, 8), 9)
);

// Elemento que será buscado na matriz
$elementoBuscado = 9;

// Chamada da função para buscar o elemento na matriz
$resultado = buscarElementoMatriz($matriz, $elementoBuscado);

// Verificação do resultado
if ($resultado) {
    echo "O elemento $elementoBuscado foi encontrado na matriz.";
} else {
    echo "O elemento $elementoBuscado não foi encontrado na matriz.";
}
?>
```

Neste código, temos uma função chamada `buscarElementoMatriz` que recebe uma matriz multidimensional e um elemento a ser buscado. A função percorre todos os valores da matriz e, se um valor for outro array, chama recursivamente a função `buscarElementoMatriz` para buscar o elemento nesse subarray. Se o valor for um elemento simples, compara-o com o elemento buscado. Se o elemento for encontrado, a função retorna `true`, caso contrário, retorna `false`.

Em seguida, temos uma matriz multidimensional de exemplo e um elemento que será buscado nessa matriz. Chamamos a função `buscarElementoMatriz` passando a matriz e o elemento buscado como parâmetros. O resultado da busca é armazenado na variável `$resultado`.

Por fim, verificamos o resultado da busca e exibimos uma mensagem correspondente. Se o elemento foi encontrado, exibimos a mensagem "O elemento [elemento buscado] foi encontrado na matriz.". Caso contrário, exibimos a mensagem "O elemento [elemento buscado] não foi encontrado na matriz.".

Esse código é um exemplo de como realizar uma busca em uma matriz multidimensional em PHP de forma recursiva e pode ser adaptado e utilizado em diferentes projetos.