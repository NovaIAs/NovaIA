Claro! Aqui está um código em Dart que cria uma lista de números primos:

```dart
import 'dart:math';

bool isPrimo(int number) {
  if (number < 2) return false;
  for (int i = 2; i <= sqrt(number); i++) {
    if (number % i == 0) return false;
  }
  return true;
}

List<int> listaPrimos(int quantidade) {
  List<int> primos = [];
  int number = 2;

  while (primos.length < quantidade) {
    if (isPrimo(number)) {
      primos.add(number);
    }
    number++;
  }

  return primos;
}

void main() {
  int quantidadePrimos = 10;
  List<int> primos = listaPrimos(quantidadePrimos);

  print('Os $quantidadePrimos primeiros números primos são:');
  for (int i = 0; i < primos.length; i++) {
    print('${i + 1}: ${primos[i]}');
  }
}
```

Neste código, começamos definindo uma função chamada `isPrimo`, que recebe um número inteiro e verifica se ele é primo ou não. A função retorna `true` se o número for primo e `false` caso contrário. 

Em seguida, temos a função `listaPrimos`, que recebe a quantidade de números primos que queremos gerar. Essa função cria uma lista vazia chamada `primos` e um contador chamado `number` que começa em 2. 

Dentro de um loop while, verificamos se o tamanho da lista `primos` é menor que a quantidade desejada. Se for, chamamos a função `isPrimo` passando o valor de `number`. Se o número for primo, adicionamos ele à lista `primos`. Em seguida, incrementamos o valor de `number` em 1. O loop continua até que a lista `primos` tenha a quantidade de números primos desejada.

No `main`, definimos a variável `quantidadePrimos` com o valor 10 (mas você pode alterar para o valor que desejar). Chamamos a função `listaPrimos`, passando `quantidadePrimos` como argumento, e guardamos o resultado na variável `primos`. 

Por fim, exibimos uma mensagem informando a quantidade de números primos gerados e percorremos a lista `primos` com um loop for, exibindo cada número primo com o seu índice na lista.

Espero que isso ajude! Se você tiver alguma dúvida específica sobre o código, estou aqui para ajudar.