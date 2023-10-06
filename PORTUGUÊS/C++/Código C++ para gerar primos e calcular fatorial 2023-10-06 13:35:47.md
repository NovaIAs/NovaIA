#include <iostream>
#include <vector>

// Função para verificar se um número é primo
bool ehPrimo(int num) {
  if (num <= 1) {
    return false;
  }
  for (int i = 2; i * i <= num; ++i) {
    if (num % i == 0) {
      return false;
    }
  }
  return true;
}

// Função para gerar os números primos até um limite
std::vector<int> gerarPrimosAte(int limite) {
  std::vector<int> primos;
  for (int i = 2; i <= limite; ++i) {
    if (ehPrimo(i)) {
      primos.push_back(i);
    }
  }
  return primos;
}

// Função para calcular o fatorial de um número
int calcularFatorial(int num) {
  int fatorial = 1;
  for (int i = 1; i <= num; ++i) {
    fatorial *= i;
  }
  return fatorial;
}

// Função principal
int main() {
  std::cout << "Digite um número limite: ";
  int limite;
  std::cin >> limite;
  
  std::vector<int> primos = gerarPrimosAte(limite);
  
  std::cout << "Números primos até " << limite << ":" << std::endl;
  for (int primo : primos) {
    std::cout << primo << " ";
  }
  std::cout << std::endl;
  
  std::cout << "Fatorial de cada número primo:" << std::endl;
  for (int primo : primos) {
    int fatorial = calcularFatorial(primo);
    std::cout << "Fatorial de " << primo << ": " << fatorial << std::endl;
  }
  
  return 0;
}

Este código em C++ é capaz de gerar e imprimir todos os números primos até um limite fornecido pelo usuário, além de calcular o fatorial de cada número primo encontrado.

A função "ehPrimo" verifica se um número é primo, retornando verdadeiro se for e falso caso contrário. Ela utiliza um laço de repetição para testar todos os possíveis divisores do número até a raiz quadrada dele.

A função "gerarPrimosAte" utiliza a função "ehPrimo" para gerar um vetor contendo todos os números primos até um determinado limite fornecido como argumento.

A função "calcularFatorial" calcula o fatorial de um número dado, utilizando um laço de repetição para multiplicar todos os números de 1 até o número dado.

Na função principal, o usuário é solicitado a digitar um número limite. Os números primos até esse limite são gerados utilizando a função "gerarPrimosAte" e são impressos na tela. Em seguida, o código calcula o fatorial de cada número primo encontrado e também os imprime.