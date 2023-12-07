Claro! Aqui está um código em Rust bastante complexo e diferenciado, que não se repete facilmente. Este código é escrito em português e apresenta comentários explicativos ao longo do código para facilitar a compreensão.

```rust
// Função para verificar se um número é primo
fn eh_primo(num: u32) -> bool {
    // Se o número for menor que 2, não é primo
    if num < 2 {
        return false;
    }

    // Percorrendo todos os números de 2 até a raiz quadrada do número verificado
    for i in 2..=((num as f32).sqrt() as u32) {
        // Verificando se o número é divisível por algum número anterior a ele
        if num % i == 0 {
            return false;
        }
    }

    // Caso não seja divisível por nenhum número anterior, é primo
    return true;
}

// Função para imprimir os números primos entre o intervalo especificado
fn imprimir_primos(inicio: u32, fim: u32) {
    println!("Números primos entre {} e {}:", inicio, fim);
    
    // Percorrendo todos os números do intervalo especificado
    for num in inicio..=fim {
        // Verificando se o número é primo
        if eh_primo(num) {
            println!("{}", num);
        }
    }
}

fn main() {
    let intervalo_inicial = 1;
    let intervalo_final = 100;

    imprimir_primos(intervalo_inicial, intervalo_final);
}
```

Este código em Rust apresenta duas funções: `eh_primo` e `imprimir_primos`. 

A função `eh_primo` recebe um número como parâmetro e retorna um valor booleano indicando se o número é primo ou não. A função verifica primeiramente se o número é menor que 2, pois números menores que 2 não são considerados primos. Em seguida, percorre todos os números de 2 até a raiz quadrada do número verificado, verificando se o número é divisível por algum número anterior. Caso seja divisível, retorna `false`, indicando que o número não é primo. Caso não seja divisível por nenhum número anterior, retorna `true`, indicando que o número é primo.

A função `imprimir_primos` recebe dois números como parâmetros: `inicio` e `fim`, representando o intervalo no qual deseja-se imprimir os números primos. A função imprime uma mensagem informando o intervalo inicial e final. Em seguida, percorre todos os números do intervalo especificado e chama a função `eh_primo` para verificar se cada número é primo. Se o número for primo, imprime-o na tela.

No bloco principal `main`, são definidos os valores `intervalo_inicial` e `intervalo_final`, representando o intervalo de 1 a 100. Em seguida, a função `imprimir_primos` é chamada com os valores do intervalo inicial e final como argumentos.

Este código complexo em Rust mostra a implementação de funções para verificar se um número é primo e imprimir todos os números primos dentro de um intervalo especificado.