Claro! Vou gerar um código em C++ complexo e explicar cada parte para você. Aqui está um exemplo:

```cpp
#include <iostream>
using namespace std;

// Função para calcular o fatorial de um número
int calcularFatorial(int num) {
    if (num == 0) {
        return 1;
    }
    else {
        return num * calcularFatorial(num - 1);
    }
}

// Função para imprimir os números primos de 1 até um número fornecido
void imprimirPrimos(int limite) {
    bool isPrimo = true;

    for (int i = 2; i <= limite; i++) {
        // Verifica se o número atual é primo
        for (int j = 2; j < i; j++) {
            if (i % j == 0) {
                isPrimo = false;
                break;
            }
        }

        // Imprime o número se for primo
        if (isPrimo) {
            cout << i << " ";
        }

        // Reinicializa o valor de isPrimo para a próxima iteração
        isPrimo = true;
    }
}

int main() {
    int numero;

    // Solicita ao usuário um número para calcular o fatorial
    cout << "Digite um número para calcular o fatorial: ";
    cin >> numero;

    // Calcula o fatorial e imprime o resultado
    int resultadoFatorial = calcularFatorial(numero);
    cout << "O fatorial de " << numero << " é: " << resultadoFatorial << endl;

    // Solicita ao usuário um número limite para imprimir os números primos
    cout << "\nDigite um número limite para imprimir os números primos: ";
    cin >> numero;

    // Imprime os números primos até o número limite fornecido
    cout << "Os números primos até " << numero << " são: ";
    imprimirPrimos(numero);

    return 0;
}
```

Explicação:

1. Na primeira linha, incluímos a biblioteca `iostream`, que permite a interação do usuário com o programa.
2. Em seguida, usamos o namespace `std` para evitar a necessidade de escrever `std::` antes de cada função da biblioteca `iostream`.
3. Definimos uma função `calcularFatorial` que recebe um número `num`. Essa função utiliza uma abordagem recursiva para calcular o fatorial do número fornecido.
4. Dentro da função `calcularFatorial`, verificamos se `num` é igual a zero. Se for, retornamos 1 (o fatorial de zero é 1). Caso contrário, multiplicamos `num` pelo resultado da chamada recursiva da função `calcularFatorial` com o argumento `num - 1`.
5. Em seguida, definimos a função `imprimirPrimos`, que recebe um número `limite` como parâmetro. Essa função irá imprimir todos os números primos de 1 até o número `limite`.
6. Dentro da função `imprimirPrimos`, inicializamos a variável booleana `isPrimo` como `true`. Essa variável será usada para verificar se um número é primo.
7. Utilizamos um loop `for` para percorrer todos os números de 2 até o `limite`.
8. Em seguida, utilizamos outro loop `for` para verificar se o número atual é divisível por algum número anterior a ele. Se for, `isPrimo` é definido como `false` e o loop secundário é interrompido.
9. Fora do loop secundário, verificamos se `isPrimo` é `true`. Se for, significa que o número é primo e o imprimimos na tela.
10. Reinicializamos o valor de `isPrimo` como `true` para a próxima iteração do loop principal.
11. No `main()`, declaramos a variável `numero`.
12. Solicitamos ao usuário que digite um número para calcular o fatorial e armazenamos o valor em `numero` usando `cin`.
13. Calculamos o fatorial do número fornecido chamando a função `calcularFatorial` e armazenamos o resultado na variável `resultadoFatorial`.
14. Imprimimos o resultado do fatorial na tela.
15. Em seguida, solicitamos ao usuário um número limite para imprimir os números primos e armazenamos o valor em `numero` usando `cin`.
16. Imprimimos os números primos até o número limite fornecido chamando a função `imprimirPrimos`.
17. Retornamos 0 para indicar que o programa foi executado com sucesso.