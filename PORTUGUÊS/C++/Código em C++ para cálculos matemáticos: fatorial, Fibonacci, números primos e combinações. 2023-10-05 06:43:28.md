#include <iostream>
#include <cmath>

using namespace std;

// Função para calcular o fatorial de um número
int fatorial(int n) {
  if (n <= 1) {
    return 1;
  }
  
  return n * fatorial(n - 1);
}

// Função para calcular o número de Fibonacci
int fibonacci(int n) {
  if (n <= 1) {
    return n;
  }
  
  return fibonacci(n - 1) + fibonacci(n - 2);
}

// Função para verificar se um número é primo
bool ehPrimo(int n) {
  if (n <= 1) {
    return false;
  }
  
  for (int i = 2; i <= sqrt(n); i++) {
    if (n % i == 0) {
      return false;
    }
  }
  
  return true;
}

// Função para calcular o número de combinações
int combinacoes(int n, int r) {
  if (r == 0 || r == n) {
    return 1;
  }
  
  return fatorial(n) / (fatorial(r) * fatorial(n - r));
}

// Função principal
int main() {
  // Solicita ao usuário o valor de n para calcular os números de Fibonacci
  int n;
  cout << "Digite um valor inteiro positivo para calcular os números de Fibonacci: ";
  cin >> n;
  
  // Calcula os números de Fibonacci até o valor informado
  cout << "Números de Fibonacci até o valor " << n << ":" << endl;
  for (int i = 0; i <= n; i++) {
    cout << fibonacci(i) << " ";
  }
  cout << endl << endl;
  
  // Solicita ao usuário o valor de n para verificar se é primo
  cout << "Digite um valor inteiro positivo para verificar se é primo: ";
  cin >> n;
  
  // Verifica se o valor informado é primo
  if (ehPrimo(n)) {
    cout << n << " é primo." << endl << endl;
  } else {
    cout << n << " não é primo." << endl << endl;
  }
  
  // Solicita ao usuário o valor de n e r para calcular as combinações
  int r;
  cout << "Digite dois valores inteiros positivos para calcular as combinações: ";
  cin >> n >> r;
  
  // Calcula as combinações utilizando a fórmula do número de combinações
  cout << "Número de combinações de " << n << " elementos tomados " << r << " a " << r << ":" << endl;
  cout << combinacoes(n, r) << endl;
  
  return 0;
}
```

Este código em C++ realiza diversas operações matemáticas. Ele começa com a definição de quatro funções: `fatorial`, `fibonacci`, `ehPrimo` e `combinacoes`.

A função `fatorial` recebe um número inteiro `n` e calcula o fatorial desse número utilizando recursão. Se `n` for menor ou igual a 1, a função retorna 1. Caso contrário, a função calcula o fatorial de `n` multiplicando-o pelo fatorial de `n-1`.

A função `fibonacci` recebe um número inteiro `n` e retorna o `n`-ésimo número da sequência de Fibonacci utilizando recursão. Se `n` for menor ou igual a 1, a função retorna `n`. Caso contrário, a função retorna a soma dos dois números anteriores da sequência.

A função `ehPrimo` recebe um número inteiro `n` e verifica se esse número é primo. Se `n` for menor ou igual a 1, a função retorna falso. Caso contrário, a função itera de 2 até a raiz quadrada de `n` para verificar se `n` é divisível por algum número. Se `n` for divisível por algum número, a função retorna falso. Caso contrário, a função retorna verdadeiro.

A função `combinacoes` recebe dois números inteiros `n` e `r` e calcula o número de combinações de `n` elementos tomados `r` a `r` utilizando a fórmula do número de combinações. Se `r` for igual a 0 ou igual a `n`, a função retorna 1. Caso contrário, a função calcula o fatorial de `n` e divide pelo produto do fatorial de `r` e do fatorial de `n-r`.

No `main`, o código solicita ao usuário um valor inteiro positivo `n` para calcular os números de Fibonacci até o valor informado. Em seguida, o código utiliza um loop `for` para chamar a função `fibonacci` e exibir os números de Fibonacci na tela.

Depois, o código solicita ao usuário um valor inteiro positivo `n` para verificar se é primo. O código chama a função `ehPrimo` e exibe na tela se o número é primo ou não.

Por fim, o código solicita ao usuário dois valores inteiros positivos `n` e `r` para calcular as combinações. O código chama a função `combinacoes` e exibe o número de combinações na tela.