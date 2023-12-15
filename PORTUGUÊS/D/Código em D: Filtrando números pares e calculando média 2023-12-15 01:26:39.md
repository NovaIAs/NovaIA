Claro! Aqui está um código complexo na linguagem D:

```d
import std.algorithm;

void main()
{
    // Define uma lista de números
    int[] numeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    
    // Filtra os números pares
    auto numerosPares = numeros.filter!(n => n % 2 == 0);
    
    // Calcula a média dos números pares
    double media = numerosPares.reduce!((a, b) => a + b) / numerosPares.length;
    
    // Ordena os números pares em ordem decrescente
    numerosPares.sort!((a, b) => b - a);
    
    // Imprime os números pares ordenados
    writeln("Números pares ordenados em ordem decrescente: ", numerosPares);
    
    // Imprime a média dos números pares
    writeln("Média dos números pares: ", media);
    
    // Verifica se o número 8 está presente na lista
    bool contemOito = numerosPares.canFind(8);
    
    if (contemOito)
    {
        writeln("A lista de números pares contém o número 8.");
    }
    else
    {
        writeln("A lista de números pares não contém o número 8.");
    }
}
```

Explicação do código:

1. Importamos o módulo `std.algorithm` para utilizar funções de manipulação de coleções.
2. Definimos uma lista de números de 1 a 10.
3. Utilizamos a função `filter` para filtrar somente os números pares da lista.
4. Utilizamos a função `reduce` para calcular a soma dos números pares e dividimos pelo comprimento da lista para obter a média.
5. Utilizamos a função `sort` para ordenar os números pares em ordem decrescente.
6. Imprimimos na tela os números pares ordenados em ordem decrescente.
7. Imprimimos na tela a média dos números pares.
8. Utilizamos a função `canFind` para verificar se o número 8 está presente na lista de números pares.
9. Dependendo do resultado, imprimimos na tela se o número 8 está ou não presente na lista de números pares.