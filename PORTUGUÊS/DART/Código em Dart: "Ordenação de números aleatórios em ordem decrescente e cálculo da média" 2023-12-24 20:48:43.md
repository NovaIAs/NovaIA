Claro! Aqui está um código complexo em Dart:

```dart
import 'dart:math';

void main() {
  // Criando uma lista de números aleatórios
  List<int> numeros = List.generate(10, (index) => Random().nextInt(100));

  // Ordenando a lista de forma decrescente
  numeros.sort((a, b) => b.compareTo(a));

  // Imprimindo os números
  print('Lista de números em ordem decrescente:');
  for (var numero in numeros) {
    print(numero);
  }

  // Verificando se a lista contém um número específico
  int numeroProcurado = 42;
  if (numeros.contains(numeroProcurado)) {
    print('A lista contém o número $numeroProcurado!');
  } else {
    print('A lista não contém o número $numeroProcurado.');
  }

  // Calculando a média dos números da lista
  int soma = 0;
  for (var numero in numeros) {
    soma += numero;
  }
  double media = soma / numeros.length;
  print('A média dos números é: $media');

  // Verificando se a lista contém apenas números pares
  bool todosPares = numeros.every((numero) => numero % 2 == 0);
  if (todosPares) {
    print('A lista contém apenas números pares.');
  } else {
    print('A lista contém números ímpares.');
  }
}
```

Neste código, criamos uma lista de 10 números aleatórios usando a classe `Random` do Dart. Em seguida, ordenamos a lista em ordem decrescente usando a função `sort` e a função `compareTo`.

Depois, imprimimos os números da lista usando um loop `for`. Em seguida, verificamos se a lista contém um número específico usando a função `contains`. Se o número estiver presente na lista, exibimos uma mensagem informando isso.

Em seguida, calculamos a média dos números da lista somando todos os elementos e dividindo pela quantidade de elementos. Para isso, usamos um loop `for` para somar os números e, em seguida, dividimos a soma pelo tamanho da lista.

Por fim, verificamos se a lista contém apenas números pares usando a função `every`. A função `every` verifica se todos os elementos da lista satisfazem uma condição específica. Neste caso, verificamos se cada número é par usando o operador `%` e, se todos os números forem pares, exibimos uma mensagem informando isso.

Espero que este código tenha atendido às suas expectativas!