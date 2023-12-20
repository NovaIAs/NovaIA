Claro! Aqui está um código complexo em DART:

```dart
import 'dart:math';

void main() {
  List<int> numeros = [];
  Random random = Random();
  
  // Gerando uma lista de 100 números aleatórios entre 1 e 1000
  for (int i = 0; i < 100; i++) {
    numeros.add(random.nextInt(1000) + 1);
  }
  
  // Ordenando a lista em ordem decrescente
  numeros.sort((a, b) => b.compareTo(a));
  
  // Calculando a média dos números da lista
  double media = numeros.reduce((a, b) => a + b) / numeros.length;
  
  // Verificando se cada número é par ou ímpar e imprimindo o resultado
  for (int numero in numeros) {
    if (numero % 2 == 0) {
      print("$numero é um número par");
    } else {
      print("$numero é um número ímpar");
    }
  }
  
  // Exibindo a média dos números da lista
  print("A média dos números é $media");
}
```

Neste código, começamos importando a biblioteca `dart:math` para podermos gerar números aleatórios. Em seguida, declaramos uma lista vazia chamada `numeros` e uma variável do tipo `Random` chamada `random` para gerar os números aleatórios.

Dentro do loop `for`, geramos 100 números aleatórios entre 1 e 1000 usando o método `nextInt`. Esses números são adicionados à lista `numeros`.

Em seguida, utilizamos o método `sort` para ordenar a lista em ordem decrescente, utilizando uma função de comparação que compara dois números e retorna um valor negativo, zero ou positivo, dependendo da ordem desejada.

Calculamos a média dos números da lista usando o método `reduce` em conjunto com a função de soma (`(a, b) => a + b`) e dividimos pelo tamanho da lista.

Por fim, percorremos a lista de números com um loop `for`, verificamos se cada número é par ou ímpar utilizando o operador `%` e imprimimos o resultado. Em seguida, exibimos a média dos números utilizando a função `print`.

Esse código é complexo porque envolve a geração de números aleatórios, ordenação de uma lista, cálculos matemáticos e manipulação de strings para exibição dos resultados.