Certamente! Aqui está um código complexo em Rust que demonstra uma implementação do algoritmo de ordenação chamado Merge Sort:

```rust
// Função merge para mesclar duas partes de um vetor ordenadas
fn merge(vetor: &mut [i32], esquerda: usize, meio: usize, direita: usize) {
    let tamanho_esquerda = meio - esquerda + 1;
    let tamanho_direita = direita - meio;

    // Cria vetores temporários para armazenar as partes esquerda e direita
    let mut vetor_esquerda = Vec::with_capacity(tamanho_esquerda);
    let mut vetor_direita = Vec::with_capacity(tamanho_direita);

    // Copia os elementos para os vetores temporários
    for i in 0..tamanho_esquerda {
        vetor_esquerda.push(vetor[esquerda + i]);
    }
    for j in 0..tamanho_direita {
        vetor_direita.push(vetor[meio + 1 + j]);
    }

    // Índices iniciais para percorrer os vetores temporários
    let mut i = 0;
    let mut j = 0;

    // Índice inicial para mesclar os elementos no vetor original
    let mut k = esquerda;

    // Mescla os vetores temporários de volta no vetor original
    while i < tamanho_esquerda && j < tamanho_direita {
        if vetor_esquerda[i] <= vetor_direita[j] {
            vetor[k] = vetor_esquerda[i];
            i += 1;
        } else {
            vetor[k] = vetor_direita[j];
            j += 1;
        }
        k += 1;
    }

    // Copia os elementos restantes do vetor esquerdo, se houver
    while i < tamanho_esquerda {
        vetor[k] = vetor_esquerda[i];
        i += 1;
        k += 1;
    }

    // Copia os elementos restantes do vetor direito, se houver
    while j < tamanho_direita {
        vetor[k] = vetor_direita[j];
        j += 1;
        k += 1;
    }
}

// Função merge_sort para dividir o vetor em partes menores e chamadas recursivas
fn merge_sort(vetor: &mut [i32], esquerda: usize, direita: usize) {
    if esquerda < direita {
        let meio = (esquerda + direita) / 2;

        // Chamada recursiva para a parte esquerda e direita do vetor
        merge_sort(vetor, esquerda, meio);
        merge_sort(vetor, meio + 1, direita);

        // Mescla as partes esquerda e direita
        merge(vetor, esquerda, meio, direita);
    }
}

fn main() {
    let mut vetor = [9, 5, 1, 8, 3, 4, 2, 7, 6];
    let tamanho = vetor.len();

    println!("Vetor original: {:?}", vetor);

    // Chama a função merge_sort para ordenar o vetor
    merge_sort(&mut vetor, 0, tamanho - 1);

    println!("Vetor ordenado: {:?}", vetor);
}
```

Este código implementa o algoritmo Merge Sort em Rust. O Merge Sort é um algoritmo de ordenação eficiente que divide um vetor em partes menores, as ordena individualmente e, em seguida, mescla as partes ordenadas para obter o vetor final ordenado.

O código começa com a definição de duas funções: `merge` e `merge_sort`. A função `merge` é responsável por mesclar duas partes do vetor ordenadas, enquanto a função `merge_sort` divide o vetor em partes menores e realiza chamadas recursivas para ordenar essas partes.

A função `merge` recebe o vetor, os índices das partes esquerda, meio e direita, e realiza a etapa de mesclagem. Ela cria vetores temporários para armazenar as partes esquerda e direita, copia os elementos relevantes para esses vetores temporários e, em seguida, mescla os elementos de volta no vetor original.

A função `merge_sort` é a função principal que inicia a ordenação. Ela recebe o vetor, os índices da parte esquerda e direita, e realiza o seguinte processo:
1. Verifica se há mais de um elemento na parte do vetor.
2. Calcula o índice do meio da parte do vetor.
3. Chama recursivamente `merge_sort` para as partes esquerda e direita do vetor.
4. Chama a função `merge` para mesclar as partes esquerda e direita.

O programa principal `main` inicializa um vetor desordenado, chama a função `merge_sort` para ordená-lo e imprime o vetor original e o vetor ordenado.

Espero que este código complexo em Rust tenha atendido às suas expectativas!