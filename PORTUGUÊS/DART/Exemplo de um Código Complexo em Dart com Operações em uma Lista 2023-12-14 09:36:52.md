Claro! Aqui está um exemplo de um código complexo em Dart:

```dart
import 'dart:math';

void main() {
  List<int> numeros = [];

  for (int i = 0; i < 10; i++) {
    numeros.add(Random().nextInt(100));
  }

  print("Lista original: $numeros");

  ordenarLista(numeros);
  print("Lista ordenada: $numeros");

  int soma = somarLista(numeros);
  print("Soma dos elementos da lista: $soma");

  List<int> numerosPares = filtrarNumerosPares(numeros);
  print("Números pares da lista: $numerosPares");

  int maiorNumero = encontrarMaiorNumero(numeros);
  print("Maior número da lista: $maiorNumero");
}

void ordenarLista(List<int> lista) {
  for (int i = 0; i < lista.length - 1; i++) {
    for (int j = 0; j < lista.length - i - 1; j++) {
      if (lista[j] > lista[j + 1]) {
        int temp = lista[j];
        lista[j] = lista[j + 1];
        lista[j + 1] = temp;
      }
    }
  }
}

int somarLista(List<int> lista) {
  int soma = 0;

  for (int i = 0; i < lista.length; i++) {
    soma += lista[i];
  }

  return soma;
}

List<int> filtrarNumerosPares(List<int> lista) {
  List<int> numerosPares = [];

  for (int i = 0; i < lista.length; i++) {
    if (lista[i] % 2 == 0) {
      numerosPares.add(lista[i]);
    }
  }

  return numerosPares;
}

int encontrarMaiorNumero(List<int> lista) {
  int maiorNumero = lista[0];

  for (int i = 1; i < lista.length; i++) {
    if (lista[i] > maiorNumero) {
      maiorNumero = lista[i];
    }
  }

  return maiorNumero;
}
```

Neste código em Dart, temos uma função principal `main()` que realiza as seguintes operações:

1. Cria uma lista vazia chamada `numeros`.
2. Utiliza um loop `for` para gerar 10 números aleatórios entre 0 e 100 e adicioná-los à lista `numeros`.
3. Imprime a lista original.
4. Chama a função `ordenarLista()` para ordenar a lista `numeros` em ordem crescente.
5. Imprime a lista ordenada.
6. Chama a função `somarLista()` para somar todos os elementos da lista `numeros`.
7. Imprime a soma.
8. Chama a função `filtrarNumerosPares()` para filtrar apenas os números pares da lista `numeros`.
9. Imprime os números pares filtrados.
10. Chama a função `encontrarMaiorNumero()` para encontrar o maior número da lista `numeros`.
11. Imprime o maior número.

As funções `ordenarLista()`, `somarLista()`, `filtrarNumerosPares()` e `encontrarMaiorNumero()` são utilizadas para modularizar e reutilizar o código, facilitando a leitura e manutenção do programa.