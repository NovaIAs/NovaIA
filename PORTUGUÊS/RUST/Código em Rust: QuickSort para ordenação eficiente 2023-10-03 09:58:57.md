Claro! Aqui está um código em Rust que implementa o algoritmo de ordenação QuickSort:

```rust
fn quicksort<T: Ord>(arr: &mut [T]) {
    if arr.len() <= 1 {
        return;
    }

    let pivot = partition(arr);

    quicksort(&mut arr[0..pivot]);
    quicksort(&mut arr[pivot + 1..]);
}

fn partition<T: Ord>(arr: &mut [T]) -> usize {
    let len = arr.len();
    let pivot_index = len / 2;
    let pivot_value = arr[pivot_index];

    arr.swap(pivot_index, len - 1);

    let mut i = 0;
    for j in 0..len - 1 {
        if arr[j] <= pivot_value {
            arr.swap(i, j);
            i += 1;
        }
    }

    arr.swap(i, len - 1);
    i
}

fn main() {
    let mut arr = [9, 5, 2, 7, 1, 10];
    quicksort(&mut arr);
    println!("{:?}", arr);
}
```

Neste código, temos uma função `quicksort` que recebe uma referência mutável para um array genérico `arr`. Primeiro, verificamos se o tamanho do array é menor ou igual a 1, caso seja, retornamos imediatamente, pois não há necessidade de ordenar.

Caso contrário, chamamos a função `partition` para encontrar o índice do pivô e particionar o array em duas partes. Em seguida, chamamos recursivamente a função `quicksort` para ordenar as duas partes do array, uma antes do pivô e outra depois.

A função `partition` encontra o índice do pivô escolhendo o elemento do meio do array. Em seguida, move o pivô para o final do array e itera pelo restante dos elementos, trocando-os de posição conforme necessário para colocar os elementos menores que o pivô à esquerda e os maiores à direita. Por fim, move o pivô para a posição correta e retorna o seu índice.

No `main`, criamos um array de exemplo `[9, 5, 2, 7, 1, 10]`, chamamos a função `quicksort` passando o array e, em seguida, imprimimos o array ordenado.