```php
// Programa para implementar el algoritmo de clasificación rápida en PHP.
// Este algoritmo es utilizado para ordenar una lista de elementos en orden ascendente o descendente.

// Función para intercambiar dos elementos en una matriz.
function swap(&$a, &$b) {
    $temp = $a;
    $a = $b;
    $b = $temp;
}

// Función para particionar la matriz en dos partes.
function partition(&$arr, $low, $high) {
    $pivot = $arr[$high];
    $i = $low - 1;

    for ($j = $low; $j < $high; $j++) {
        if ($arr[$j] < $pivot) {
            $i++;
            swap($arr[$i], $arr[$j]);
        }
    }

    swap($arr[$i + 1], $arr[$high]);

    return $i + 1;
}

// Función recursiva para ordenar la matriz.
function quickSort(&$arr, $low, $high) {
    if ($low < $high) {
        $partitionIndex = partition($arr, $low, $high);

        quickSort($arr, $low, $partitionIndex - 1);
        quickSort($arr, $partitionIndex + 1, $high);
    }
}

// Definir la matriz a ordenar.
$arr = array(10, 7, 8, 9, 1, 5);

// Imprimir la matriz original.
echo "Matriz original: ";
for ($i = 0; $i < count($arr); $i++) {
    echo $arr[$i] . " ";
}
echo "\n";

// Ordenar la matriz usando el algoritmo de clasificación rápida.
quickSort($arr, 0, count($arr) - 1);

// Imprimir la matriz ordenada.
echo "Matriz ordenada: ";
for ($i = 0; $i < count($arr); $i++) {
    echo $arr[$i] . " ";
}
```

**Explicación del código:**

1. La función `swap()` intercambia dos elementos en una matriz.

2. La función `partition()` particiona la matriz en dos partes: una parte que contiene elementos menores que el pivote y otra parte que contiene elementos mayores o iguales que el pivote. La función devuelve el índice del pivote.

3. La función `quickSort()` ordena la matriz usando el algoritmo de clasificación rápida. La función divide repetidamente la matriz en dos partes hasta que todos los elementos estén ordenados.

4. La matriz a ordenar se define como `$arr`.

5. La matriz original se imprime usando un bucle `for`.

6. La matriz se ordena usando la función `quickSort()`.

7. La matriz ordenada se imprime usando un bucle `for`.