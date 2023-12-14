#include <stdio.h>

// Função para verificar se um número é primo
int ehPrimo(int num) {
    if (num <= 1) {
        return 0;
    }
    
    for (int i = 2; i <= num / 2; ++i) {
        if (num % i == 0) {
            return 0;
        }
    }
    
    return 1;
}

// Função para encontrar o próximo número primo a partir de um determinado número
int proximoPrimo(int num) {
    int proximo = num + 1;
    
    while (1) {
        if (ehPrimo(proximo)) {
            return proximo;
        }
        proximo++;
    }
}

int main() {
    int numero;
    
    printf("Digite um número: ");
    scanf("%d", &numero);
    
    int proximo = proximoPrimo(numero);
    
    printf("O próximo número primo a partir de %d é %d.\n", numero, proximo);
    
    return 0;
}

Neste código em C, criei uma função chamada "ehPrimo" que verifica se um número é primo. A função retorna 1 se o número for primo e 0 caso contrário.

Em seguida, criei outra função chamada "proximoPrimo" que encontra o próximo número primo a partir de um determinado número. Essa função recebe como parâmetro o número a partir do qual se quer encontrar o próximo primo e retorna o próximo número primo encontrado.

No programa principal, solicito ao usuário que digite um número e armazeno o valor na variável "numero". Em seguida, chamo a função "proximoPrimo" passando o número digitado como argumento e armazeno o resultado na variável "proximo".

Por fim, exibo na tela o próximo número primo encontrado a partir do número digitado pelo usuário.