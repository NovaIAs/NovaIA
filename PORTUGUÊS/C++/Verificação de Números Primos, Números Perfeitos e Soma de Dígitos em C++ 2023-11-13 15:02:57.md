#include <iostream>
#include <cmath>

using namespace std;

// Função para verificar se um número é primo
bool verificaPrimo(int n) {
    if (n <= 1)
        return false;
    if (n == 2)
        return true;
    if (n % 2 == 0)
        return false;

    int limite = sqrt(n) + 1;
    for (int i = 3; i < limite; i += 2) {
        if (n % i == 0)
            return false;
    }

    return true;
}

// Função para verificar se um número é perfeito
bool verificaPerfeito(int n) {
    if (n <= 1)
        return false;

    int somaDivisores = 1;

    for (int i = 2; i <= n / 2; i++) {
        if (n % i == 0)
            somaDivisores += i;
    }

    return somaDivisores == n;
}

// Função para encontrar o próximo número perfeito a partir de um determinado número
int encontraProximoPerfeito(int n) {
    int i = n + 1;

    while (true) {
        if (verificaPerfeito(i))
            return i;
        i++;
    }
}

// Função para calcular a soma dos dígitos de um número
int somaDigitos(int n) {
    int soma = 0;

    while (n != 0) {
        soma += n % 10;
        n /= 10;
    }

    return soma;
}

int main() {
    // Solicita ao usuário um número inteiro positivo
    int numero;
    cout << "Digite um numero inteiro positivo: ";
    cin >> numero;

    // Verifica se o número é primo
    if (verificaPrimo(numero))
        cout << "O numero digitado eh primo." << endl;
    else
        cout << "O numero digitado nao eh primo." << endl;

    // Verifica se o número é perfeito
    if (verificaPerfeito(numero))
        cout << "O numero digitado eh perfeito." << endl;
    else
        cout << "O numero digitado nao eh perfeito." << endl;

    // Encontra o próximo número perfeito após o número digitado
    int proximoPerfeito = encontraProximoPerfeito(numero);
    cout << "O proximo numero perfeito a partir do numero digitado eh: " << proximoPerfeito << endl;

    // Calcula a soma dos dígitos do número
    int soma = somaDigitos(numero);
    cout << "A soma dos digitos do numero digitado eh: " << soma << endl;

    return 0;
}