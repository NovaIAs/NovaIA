Claro! Aqui está um exemplo de um código complexo em Dart:

```dart
import 'dart:math';

void main() {
  List<int> numeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
  
  int soma = calcularSoma(numeros);
  print("A soma dos números é: $soma");
  
  int maiorNumero = encontrarMaiorNumero(numeros);
  print("O maior número é: $maiorNumero");
  
  List<int> numerosPares = encontrarNumerosPares(numeros);
  print("Os números pares são: $numerosPares");
  
  List<int> numerosPrimos = encontrarNumerosPrimos(numeros);
  print("Os números primos são: $numerosPrimos");
}

int calcularSoma(List<int> numeros) {
  int soma = 0;
  
  for (int numero in numeros) {
    soma += numero;
  }
  
  return soma;
}

int encontrarMaiorNumero(List<int> numeros) {
  int maiorNumero = numeros[0];
  
  for (int i = 1; i < numeros.length; i++) {
    if (numeros[i] > maiorNumero) {
      maiorNumero = numeros[i];
    }
  }
  
  return maiorNumero;
}

List<int> encontrarNumerosPares(List<int> numeros) {
  List<int> numerosPares = [];
  
  for (int numero in numeros) {
    if (numero % 2 == 0) {
      numerosPares.add(numero);
    }
  }
  
  return numerosPares;
}

List<int> encontrarNumerosPrimos(List<int> numeros) {
  List<int> numerosPrimos = [];
  
  for (int numero in numeros) {
    if (ehPrimo(numero)) {
      numerosPrimos.add(numero);
    }
  }
  
  return numerosPrimos;
}

bool ehPrimo(int numero) {
  if (numero < 2) {
    return false;
  }
  
  for (int i = 2; i <= sqrt(numero); i++) {
    if (numero % i == 0) {
      return false;
    }
  }
  
  return true;
}
```

Neste código em Dart, começamos importando a biblioteca 'dart:math' para facilitar o cálculo de números primos. Em seguida, definimos a função principal `main()`. Nessa função, criamos uma lista de números e chamamos as outras funções para calcular a soma dos números, encontrar o maior número, encontrar os números pares e encontrar os números primos.

A função `calcularSoma()` recebe a lista de números como parâmetro e calcula a soma de todos os elementos usando um loop `for`.

A função `encontrarMaiorNumero()` recebe a lista de números como parâmetro e encontra o maior número iterando sobre a lista e comparando cada número com o atual maior número.

A função `encontrarNumerosPares()` recebe a lista de números como parâmetro e retorna uma nova lista contendo apenas os números pares.

A função `encontrarNumerosPrimos()` recebe a lista de números como parâmetro e retorna uma nova lista contendo apenas os números primos. Essa função utiliza a função `ehPrimo()` para verificar se um número é primo ou não.

A função `ehPrimo()` recebe um número como parâmetro e verifica se ele é primo ou não, utilizando um loop `for` para testar se o número é divisível por algum número entre 2 e a raiz quadrada do próprio número.

No final, na função `main()`, imprimimos os resultados das operações utilizando a função `print()`.