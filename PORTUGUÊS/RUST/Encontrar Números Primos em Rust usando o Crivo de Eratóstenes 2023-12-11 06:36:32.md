Claro! Aqui está um exemplo de código complexo em Rust que busca números primos usando o algoritmo Crivo de Eratóstenes:

```rust
fn encontrar_primos(n: usize) -> Vec<usize> {
    let mut numeros = vec![true; n + 1];
    numeros[0] = false;
    numeros[1] = false;

    for i in 2..=n {
        if numeros[i] {
            for j in (i * i..=n).step_by(i) {
                numeros[j] = false;
            }
        }
    }

    let primos: Vec<usize> = numeros.iter()
        .enumerate()
        .filter(|&(_, &is_primo)| is_primo)
        .map(|(i, _)| i)
        .collect();

    primos
}

fn main() {
    let n = 100;
    let primos = encontrar_primos(n);
    println!("Números primos até {}: {:?}", n, primos);
}
```

Neste código, temos a função `encontrar_primos` que recebe um número `n` como argumento e retorna um vetor contendo todos os números primos até `n`. 

Primeiro, criamos um vetor `numeros` com tamanho `n+1` e inicializamos todos os elementos como `true`, indicando que eles são potenciais números primos. Em seguida, definimos os valores para `numeros[0]` e `numeros[1]` como `false`, pois eles não são primos.

A partir do número 2, percorremos todos os números até `n`. Se `numeros[i]` for verdadeiro, significa que `i` é um número primo. Portanto, iteramos por todos os múltiplos de `i` e definimos seus valores em `numeros` como `false`, pois eles não são primos.

No final, usamos o método `iter().enumerate().filter().map().collect()` para filtrar apenas os números primos e armazená-los no vetor `primos`. Em seguida, imprimimos os números primos encontrados.

No `main()`, chamamos a função `encontrar_primos` passando o valor 100 como exemplo. O resultado será a lista de números primos até 100.