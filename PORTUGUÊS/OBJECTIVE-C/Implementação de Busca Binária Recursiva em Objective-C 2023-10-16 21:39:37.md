Claro! Aqui está um código em Objective-C que implementa um algoritmo de busca binária recursiva em um array ordenado.

```objective-c
#import <Foundation/Foundation.h>

// Função para realizar a busca binária recursiva
int buscaBinariaRecursiva(NSArray *array, int inicio, int fim, int elemento) {
    // Caso base: elemento não está presente no array
    if (inicio > fim) {
        return -1;
    }
    
    // Encontra o elemento do meio do array
    int meio = (inicio + fim) / 2;
    int valorMeio = [array[meio] intValue];
    
    // Caso base: elemento encontrado
    if (valorMeio == elemento) {
        return meio;
    }
    
    // Caso recursivo: elemento está na metade esquerda do array
    if (valorMeio > elemento) {
        return buscaBinariaRecursiva(array, inicio, meio - 1, elemento);
    }
    
    // Caso recursivo: elemento está na metade direita do array
    return buscaBinariaRecursiva(array, meio + 1, fim, elemento);
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Array ordenado
        NSArray *array = @[@1, @2, @3, @4, @5, @6, @7, @8, @9, @10];
        
        // Elemento a ser buscado no array
        int elemento = 6;
        
        // Chamada da função de busca binária recursiva
        int resultado = buscaBinariaRecursiva(array, 0, (int)array.count - 1, elemento);
        
        // Verifica se o elemento foi encontrado ou não
        if (resultado != -1) {
            NSLog(@"O elemento %d foi encontrado na posição %d.", elemento, resultado);
        } else {
            NSLog(@"O elemento %d não foi encontrado no array.", elemento);
        }
    }
    return 0;
}
```

Neste código, temos uma função `buscaBinariaRecursiva` que implementa a busca binária em um array dado. A função recebe como parâmetros o array, o índice de início, o índice de fim e o elemento a ser buscado. Inicialmente, a função verifica se o índice de início é maior do que o índice de fim, o que indica que o elemento não está presente no array, retornando -1 nesse caso.

Em seguida, a função encontra o elemento do meio do array e verifica se é igual ao elemento buscado. Se for igual, retorna a posição do elemento.

Caso contrário, se o elemento do meio for maior do que o elemento buscado, a função chama recursivamente a si mesma passando a metade esquerda do array. Caso contrário, chama recursivamente a si mesma passando a metade direita do array.

No `main`, temos um exemplo de uso da função, onde é criado um array ordenado e um elemento a ser buscado. Em seguida, é chamada a função `buscaBinariaRecursiva` passando o array, os índices de início e fim do array e o elemento buscado. O resultado é armazenado em uma variável e, então, é verificado se o elemento foi encontrado ou não, exibindo a mensagem correspondente no console.

Espero que esse código atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais ajuda, fique à vontade para perguntar.