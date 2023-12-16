#include <iostream>
#include <vector>
#include <algorithm>

// Função auxiliar para calcular o fatorial de um número
int fatorial(int n) {
    if (n == 0 || n == 1) {
        return 1;
    }
    return n * fatorial(n - 1);
}

// Função auxiliar para inverter um número
int inverterNumero(int n) {
    int invertido = 0;
    while (n > 0) {
        invertido = invertido * 10 + n % 10;
        n /= 10;
    }
    return invertido;
}

// Função para verificar se um número é primo
bool ehPrimo(int n) {
    if (n <= 1) {
        return false;
    }
    for (int i = 2; i * i <= n; i++) {
        if (n % i == 0) {
            return false;
        }
    }
    return true;
}

// Função principal
int main() {
    // Vetor para armazenar os números gerados
    std::vector<int> numeros;

    // Gerando os números
    for (int i = 1; i <= 1000; i++) {
        if (i % 2 == 0 && i % 3 == 0) {
            numeros.push_back(fatorial(inverterNumero(i)));
        } else if (i % 2 == 0) {
            numeros.push_back(i * i);
        } else if (i % 3 == 0) {
            numeros.push_back(i + 10);
        } else if (ehPrimo(i)) {
            numeros.push_back(i / 2);
        } else {
            numeros.push_back(i);
        }
    }

    // Ordenando os números em ordem decrescente
    std::sort(numeros.rbegin(), numeros.rend());

    // Imprimindo os números gerados
    std::cout << "Números gerados:" << std::endl;
    for (int numero : numeros) {
        std::cout << numero << " ";
    }
    std::cout << std::endl;

    return 0;
}
```

Neste código em C++, criamos um programa que gera uma sequência de números baseado em algumas condições. A ideia é gerar uma lista de números até 1000, aplicando diferentes operações em cada número. Vamos explicar cada parte do código:

1. Incluímos as bibliotecas `<iostream>`, `<vector>` e `<algorithm>`. A primeira é necessária para input/output. A segunda é utilizada para armazenar os números gerados em um vetor. A terceira é utilizada para ordenar o vetor.

2. A função `fatorial` calcula o fatorial de um número utilizando recursão. Se o número for 0 ou 1, retorna 1. Caso contrário, retorna o número multiplicado pelo fatorial do número anterior.

3. A função `inverterNumero` recebe um número e inverte a ordem dos seus dígitos. Utilizamos o operador `%` para obter o último dígito do número e o operador `/` para remover o último dígito do número.

4. A função `ehPrimo` verifica se um número é primo. Se o número for menor ou igual a 1, retorna falso. Caso contrário, verifica se algum número de 2 até a raiz quadrada do número divide o número sem deixar resto. Se encontrar algum divisor, retorna falso. Caso contrário, retorna verdadeiro.

5. Na função principal `main`, criamos um vetor `numeros` para armazenar os números gerados.

6. Utilizamos um loop `for` para iterar de 1 até 1000.

7. Dentro do loop, aplicamos diferentes operações em cada número. Se o número for par e divisível por 3, calculamos o fatorial do número invertido. Se o número for apenas par, calculamos o quadrado do número. Se o número for apenas divisível por 3, somamos 10 ao número. Se o número for primo, dividimos por 2. Caso contrário, mantemos o número original.

8. Os números gerados são adicionados ao vetor `numeros` utilizando a função `push_back`.

9. Após o loop, utilizamos a função `sort` da biblioteca `<algorithm>` para ordenar o vetor em ordem decrescente. Utilizamos `rbegin()` e `rend()` para especificar que queremos ordenar do último elemento até o primeiro.

10. Por fim, imprimimos os números gerados na tela utilizando um loop `for` e a função `cout` da biblioteca `<iostream>`. Cada número é separado por um espaço.

Espero que esse código atenda às suas expectativas!