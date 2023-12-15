Claro! Aqui está um exemplo de código em Dart que cria uma lista de números primos e calcula a média deles:

```dart
import 'dart:math';

bool isPrimo(int num) {
  if (num < 2) return false;

  for (int i = 2; i <= sqrt(num); i++) {
    if (num % i == 0) return false;
  }

  return true;
}

double calcularMediaPrimos(int quantidade) {
  List<int> primos = [];
  int num = 2;

  while (primos.length < quantidade) {
    if (isPrimo(num)) {
      primos.add(num);
    }
    num++;
  }

  double media = primos.reduce((a, b) => a + b) / primos.length;
  return media;
}

void main() {
  int quantidade = 10;
  double mediaPrimos = calcularMediaPrimos(quantidade);

  print('A média dos primeiros $quantidade números primos é: $mediaPrimos');
}
```

Neste código, a função `isPrimo` verifica se um número é primo ou não. Ela itera de 2 até a raiz quadrada do número e verifica se há algum divisor. Se não houver, o número é considerado primo.

A função `calcularMediaPrimos` recebe como parâmetro a quantidade de números primos que se deseja calcular. Ela cria uma lista vazia chamada `primos` e um contador `num` iniciado em 2. Enquanto a quantidade de números primos na lista for menor que o parâmetro `quantidade`, a função verifica se o número atual (`num`) é primo utilizando a função `isPrimo`. Se for, o número é adicionado à lista. Depois que a lista atingir a quantidade desejada, a função calcula a média dos números primos utilizando o método `reduce` do Dart.

No método `main`, definimos a variável `quantidade` como 10 e chamamos a função `calcularMediaPrimos` com esse valor. Em seguida, o resultado é impresso no console.

Espero que este exemplo complexo e diferenciado em Dart atenda às suas expectativas!