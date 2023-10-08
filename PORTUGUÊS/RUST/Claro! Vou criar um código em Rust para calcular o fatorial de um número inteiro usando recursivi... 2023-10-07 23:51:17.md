Claro! Vou criar um código complexo em Rust que envolve um algoritmo de ordenação chamado Bucket Sort. O Bucket Sort é um algoritmo de classificação eficiente para distribuir elementos em diferentes baldes e, em seguida, classificá-los individualmente.

Aqui está o código em Rust:

```rust
fn bucket_sort(arr: &mut [f64]) {
    let len = arr.len();
    if len == 0 {
        return;
    }

    // Encontrar o valor máximo e mínimo no array
    let max_val = *arr.iter().max_by(|x, y| x.partial_cmp(y).unwrap()).unwrap();
    let min_val = *arr.iter().min_by(|x, y| x.partial_cmp(y).unwrap()).unwrap();

    // Calcular o tamanho do balde e o número de baldes
    let bucket_size = 10;
    let num_buckets = ((max_val - min_val) / bucket_size as f64).ceil() as usize;

    // Criar baldes vazios
    let mut buckets: Vec<Vec<f64>> = vec![Vec::new(); num_buckets];

    // Colocar os elementos nos baldes corretos
    for &num in arr.iter() {
        let bucket_index = ((num - min_val) / bucket_size as f64).floor() as usize;
        buckets[bucket_index].push(num);
    }

    // Ordenar cada balde e atualizar o array original
    let mut idx = 0;
    for bucket in buckets.iter_mut() {
        bucket.sort_by(|a, b| a.partial_cmp(b).unwrap());
        for &num in bucket.iter() {
            arr[idx] = num;
            idx += 1;
        }
    }
}

fn main() {
    let mut arr = vec![0.3, 0.1, 0.8, 0.5, 0.2, 0.7, 0.9, 0.4, 0.6];
    bucket_sort(&mut arr);
    println!("Array ordenado: {:?}", arr);
}
```

Neste código, a função `bucket_sort` implementa o algoritmo de ordenação Bucket Sort. Ela recebe um slice mutável de números de ponto flutuante (`&mut [f64]`) como entrada e realiza a ordenação diretamente no array fornecido.

O algoritmo começa calculando o valor máximo e mínimo no array para determinar o tamanho dos baldes. Em seguida, cria um vetor de baldes vazios com base no número de baldes necessários.

Em seguida, cada elemento do array de entrada é colocado no balde correto com base em seu valor, usando uma função de mapeamento simples. Em seguida, cada balde é ordenado individualmente e os elementos são colocados de volta no array original, resultando em um array ordenado.

No `main`, um exemplo de array é criado e passado para a função `bucket_sort`. O resultado é então impresso no console.

Espero que este código atenda às suas expectativas!