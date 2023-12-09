Claro! Aqui está um exemplo de código complexo em C++, que realiza uma série de operações matemáticas e manipulações de strings:

```cpp
#include <iostream>
#include <cmath>
#include <string>
using namespace std;

// Função para calcular o fatorial de um número
int fatorial(int n) {
    if (n == 0 || n == 1)
        return 1;
    else
        return n * fatorial(n - 1);
}

// Função para converter um número decimal para binário
string decimalParaBinario(int num) {
    string binario = "";
    while (num > 0) {
        binario = to_string(num % 2) + binario;
        num /= 2;
    }
    return binario;
}

// Função para verificar se uma string é um palíndromo
bool ehPalindromo(string str) {
    int i = 0, j = str.length() - 1;
    while (i < j) {
        if (str[i] != str[j])
            return false;
        i++;
        j--;
    }
    return true;
}

int main() {
    int n;
    cout << "Digite um número inteiro: ";
    cin >> n;

    cout << "O fatorial de " << n << " é: " << fatorial(n) << endl;

    cout << "O número " << n << " em binário é: " << decimalParaBinario(n) << endl;

    string palavra;
    cout << "Digite uma palavra ou frase: ";
    cin.ignore();
    getline(cin, palavra);

    if (ehPalindromo(palavra))
        cout << "A palavra/frase é um palíndromo." << endl;
    else
        cout << "A palavra/frase não é um palíndromo." << endl;

    double raio;
    cout << "Digite o raio de um círculo: ";
    cin >> raio;

    cout << "A área do círculo é: " << M_PI * pow(raio, 2) << endl;
    cout << "O perímetro do círculo é: " << 2 * M_PI * raio << endl;

    return 0;
}
```

Este código realiza as seguintes tarefas:

1. Calcula o fatorial de um número inteiro.
2. Converte um número decimal para binário.
3. Verifica se uma palavra ou frase é um palíndromo.
4. Calcula a área e o perímetro de um círculo, com base no raio fornecido pelo usuário.

Ao executar o programa, o usuário será solicitado a fornecer um número inteiro, uma palavra ou frase, e o raio de um círculo. Em seguida, o programa realizará os cálculos necessários e exibirá os resultados na tela.

Espero que este código complexo em C++ atenda às suas expectativas!