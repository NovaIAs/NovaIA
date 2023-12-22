Claro! Aqui está um exemplo de um código complexo em C++:

```cpp
#include <iostream>
#include <vector>

// Função para encontrar o fatorial de um número
unsigned long long int fatorial(int n) {
    unsigned long long int resultado = 1;
    
    for(int i = 1; i <= n; i++) {
        resultado *= i;
    }
    
    return resultado;
}

// Função para verificar se um número é primo
bool ehPrimo(int num) {
    if(num <= 1) {
        return false;
    }
    
    for(int i = 2; i * i <= num; i++) {
        if(num % i == 0) {
            return false;
        }
    }
    
    return true;
}

// Função para imprimir os números primos até um determinado limite
void imprimirPrimosAte(int limite) {
    std::vector<int> primos;
    
    for(int i = 2; i <= limite; i++) {
        if(ehPrimo(i)) {
            primos.push_back(i);
        }
    }
    
    std::cout << "Números primos até " << limite << ": ";
    
    for(int i = 0; i < primos.size(); i++) {
        std::cout << primos[i] << " ";
    }
    
    std::cout << std::endl;
}

int main() {
    int numero;
    
    std::cout << "Digite um número inteiro: ";
    std::cin >> numero;
    
    unsigned long long int resultadoFatorial = fatorial(numero);
    
    std::cout << "O fatorial de " << numero << " é: " << resultadoFatorial << std::endl;
    
    imprimirPrimosAte(numero);
    
    return 0;
}
```

Neste código, temos três funções principais:

1. `fatorial`: Essa função recebe um número inteiro `n` e retorna o fatorial desse número. Ela utiliza um laço de repetição `for` para calcular o fatorial.
2. `ehPrimo`: Essa função recebe um número inteiro `num` e verifica se ele é primo. Ela utiliza um laço de repetição `for` para fazer a verificação.
3. `imprimirPrimosAte`: Essa função recebe um número inteiro `limite` e imprime todos os números primos até esse limite. Ela utiliza a função `ehPrimo` para verificar se cada número é primo e armazena os números primos em um vetor.

No `main`, o programa solicita ao usuário que digite um número inteiro. Em seguida, calcula o fatorial desse número utilizando a função `fatorial` e imprime o resultado. Por fim, chama a função `imprimirPrimosAte` para imprimir os números primos até o número digitado pelo usuário.

Espero que este código atenda às suas expectativas!