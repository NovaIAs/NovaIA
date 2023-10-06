#include <iostream>
#include <vector>

using namespace std;

// Função para calcular o fatorial de um número
int fatorial(int n) {
    if (n == 0 || n == 1) {
        return 1;
    } else {
        return n * fatorial(n - 1);
    }
}

// Função para verificar se um número é primo
bool ehPrimo(int n) {
    if (n <= 1) {
        return false;
    }
    
    for (int i = 2; i <= n/2; i++) {
        if (n % i == 0) {
            return false;
        }
    }
    
    return true;
}

// Função para encontrar o maior número primo em um vetor
int encontrarMaiorPrimo(vector<int>& vetor) {
    int maiorPrimo = -1;
    
    for (int i = 0; i < vetor.size(); i++) {
        if (ehPrimo(vetor[i]) && vetor[i] > maiorPrimo) {
            maiorPrimo = vetor[i];
        }
    }
    
    return maiorPrimo;
}

// Função main
int main() {
    int n;
    cout << "Digite um número inteiro positivo: ";
    cin >> n;
    
    vector<int> numeros;
    
    // Preenche o vetor com os fatoriais dos números de 1 a n
    for (int i = 1; i <= n; i++) {
        numeros.push_back(fatorial(i));
    }
    
    // Imprime os fatoriais
    cout << "Fatoriais: ";
    for (int i = 0; i < numeros.size(); i++) {
        cout << numeros[i] << " ";
    }
    cout << endl;
    
    // Encontra o maior número primo no vetor de fatoriais
    int maiorPrimo = encontrarMaiorPrimo(numeros);
    
    if (maiorPrimo != -1) {
        cout << "O maior número primo encontrado nos fatoriais é: " << maiorPrimo << endl;
    } else {
        cout << "Não foi encontrado nenhum número primo nos fatoriais." << endl;
    }
    
    return 0;
}
```

Este código em C++ é um exemplo de um programa que recebe um número inteiro positivo do usuário e calcula os fatoriais de todos os números de 1 até o número digitado. Em seguida, ele verifica o maior número primo encontrado entre os fatoriais e exibe o resultado.

A função `fatorial` é uma função recursiva que calcula o fatorial de um número dado. Ela retorna 1 quando o número é 0 ou 1, e caso contrário, ela retorna o número multiplicado pelo fatorial do número anterior.

A função `ehPrimo` verifica se um número é primo. Ela retorna falso se o número for menor ou igual a 1. Caso contrário, ela percorre os números de 2 até a metade do número e verifica se ele é divisível por algum desses números. Se for divisível, retorna falso, caso contrário, retorna verdadeiro.

A função `encontrarMaiorPrimo` recebe um vetor de números e encontra o maior número primo presente no vetor. Ela percorre o vetor e, para cada número, verifica se ele é primo e se é maior que o maior número primo encontrado até o momento. Se for, atualiza o valor do maior número primo.

A função `main` é a função principal do programa. Ela solicita ao usuário que digite um número inteiro positivo e armazena o valor digitado na variável `n`. Em seguida, cria um vetor vazio chamado `numeros` para armazenar os fatoriais.

O programa preenche o vetor `numeros` com os fatoriais dos números de 1 até `n` usando um loop for. Depois, imprime os fatoriais na tela.

Em seguida, chama a função `encontrarMaiorPrimo` passando o vetor de fatoriais como parâmetro e armazena o resultado na variável `maiorPrimo`. Se o valor retornado for diferente de -1, significa que foi encontrado um número primo no vetor de fatoriais, então o programa exibe o maior número primo encontrado. Caso contrário, exibe uma mensagem informando que nenhum número primo foi encontrado.

Por fim, o programa retorna 0, indicando que a execução ocorreu sem erros.