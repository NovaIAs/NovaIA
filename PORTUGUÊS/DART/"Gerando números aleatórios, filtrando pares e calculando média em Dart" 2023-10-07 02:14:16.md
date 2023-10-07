Claro! Aqui está um exemplo de um código complexo em Dart:

```dart
import 'dart:math';

void main() {
  // Criando uma lista de números aleatórios
  List<int> numerosAleatorios = gerarNumerosAleatorios(10);

  // Filtrando os números pares
  List<int> numerosPares = filtrarNumerosPares(numerosAleatorios);

  // Ordenando a lista de números pares em ordem decrescente
  ordenarNumerosDecrescente(numerosPares);

  // Calculando a média dos números pares
  double media = calcularMedia(numerosPares);

  // Exibindo os resultados
  print("Lista de números aleatórios: $numerosAleatorios");
  print("Lista de números pares: $numerosPares");
  print("Lista de números pares ordenados em ordem decrescente: $numerosPares");
  print("Média dos números pares: $media");
}

List<int> gerarNumerosAleatorios(int quantidade) {
  Random random = Random();
  List<int> numeros = [];

  for (int i = 0; i < quantidade; i++) {
    numeros.add(random.nextInt(100));
  }

  return numeros;
}

List<int> filtrarNumerosPares(List<int> numeros) {
  List<int> numerosPares = [];

  for (int numero in numeros) {
    if (numero % 2 == 0) {
      numerosPares.add(numero);
    }
  }

  return numerosPares;
}

void ordenarNumerosDecrescente(List<int> numeros) {
  numeros.sort((a, b) => b.compareTo(a));
}

double calcularMedia(List<int> numeros) {
  int soma = numeros.reduce((a, b) => a + b);
  return soma / numeros.length;
}
```

Nesse exemplo, o código em Dart começa importando a biblioteca `dart:math`, que é usada para gerar números aleatórios. 

A função `main()` é o ponto de entrada do programa. Ela chama outras funções para gerar uma lista de números aleatórios, filtrar os números pares, ordenar a lista em ordem decrescente e calcular a média dos números pares.

A função `gerarNumerosAleatorios()` utiliza um objeto `Random` para gerar números aleatórios entre 0 e 100. Ela retorna uma lista com a quantidade especificada de números.

A função `filtrarNumerosPares()` recebe uma lista de números e retorna uma nova lista contendo apenas os números pares.

A função `ordenarNumerosDecrescente()` recebe uma lista de números e utiliza o método `sort()` para ordená-la em ordem decrescente.

A função `calcularMedia()` recebe uma lista de números e utiliza o método `reduce()` para somar os elementos da lista. Em seguida, divide-se a soma pelo tamanho da lista para obter a média.

Por fim, a função `main()` exibe os resultados na tela utilizando a função `print()`.