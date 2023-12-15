Claro! Aqui está um exemplo de um código complexo em C++, que foi criado especificamente para esse pedido e está em português.

```cpp
#include <iostream>
#include <vector>

using namespace std;

// Função para verificar se um número é primo
bool ehPrimo(int numero) {
    if (numero <= 1) {
        return false;
    }
    
    for (int i = 2; i * i <= numero; i++) {
        if (numero % i == 0) {
            return false;
        }
    }
    
    return true;
}

// Função para calcular o fatorial de um número
int calcularFatorial(int numero) {
    int fatorial = 1;
    
    for (int i = 1; i <= numero; i++) {
        fatorial *= i;
    }
    
    return fatorial;
}

// Função para verificar se um número é perfeito
bool ehPerfeito(int numero) {
    int somaDivisores = 0;
    
    for (int i = 1; i < numero; i++) {
        if (numero % i == 0) {
            somaDivisores += i;
        }
    }
    
    return (somaDivisores == numero);
}

int main() {
    int n;
    cout << "Digite um número inteiro positivo: ";
    cin >> n;
    
    // Verifica se o número é primo
    if (ehPrimo(n)) {
        cout << n << " é um número primo." << endl;
    } else {
        cout << n << " não é um número primo." << endl;
    }
    
    // Calcula o fatorial do número
    int fatorial = calcularFatorial(n);
    cout << "O fatorial de " << n << " é " << fatorial << endl;
    
    // Verifica se o número é perfeito
    if (ehPerfeito(n)) {
        cout << n << " é um número perfeito." << endl;
    } else {
        cout << n << " não é um número perfeito." << endl;
    }
    
    return 0;
}
```

Esse código em C++ consiste em três funções e uma função principal (main). A primeira função, `ehPrimo`, verifica se um número é primo, retornando true se for e false caso contrário. A segunda função, `calcularFatorial`, calcula o fatorial de um número, retornando o resultado. A terceira função, `ehPerfeito`, verifica se um número é perfeito, retornando true se for e false caso contrário.

Na função principal, o usuário é solicitado a inserir um número inteiro positivo. Em seguida, o código chama as três funções mencionadas acima e exibe os resultados na saída padrão. Primeiro, é verificado se o número é primo ou não. Em seguida, é calculado o fatorial do número inserido. Por fim, é verificado se o número é perfeito ou não.

Espero que esse código atenda às suas expectativas!