Com certeza! Aqui está um código complexo em Dart que envolve a manipulação de listas e o uso de funções assíncronas:

```dart
import 'dart:async';

void main() async {
  final listaNumeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

  final listaPares = await filtrarPares(listaNumeros);
  final listaImpares = await filtrarImpares(listaNumeros);

  print('Números pares: $listaPares');
  print('Números ímpares: $listaImpares');
}

Future<List<int>> filtrarPares(List<int> lista) async {
  final pares = <int>[];

  for (var numero in lista) {
    if (numero % 2 == 0) {
      await Future.delayed(Duration(seconds: 1));
      pares.add(numero);
    }
  }

  return pares;
}

Future<List<int>> filtrarImpares(List<int> lista) async {
  final impares = <int>[];

  for (var numero in lista) {
    if (numero % 2 != 0) {
      await Future.delayed(Duration(seconds: 1));
      impares.add(numero);
    }
  }

  return impares;
}
```

Neste código, temos uma função principal `main()` que utiliza duas funções assíncronas para filtrar os números pares e ímpares de uma lista de números. 

A função `filtrarPares()` recebe a lista de números, inicializa uma lista vazia chamada `pares` e percorre cada número da lista original. Se o número for par, é adicionado à lista `pares`, com um pequeno atraso de 1 segundo simulado pela função `Future.delayed()`. No final, a função retorna a lista `pares`.

A função `filtrarImpares()` segue a mesma lógica da função `filtrarPares()`, mas filtra os números ímpares.

Na função principal `main()`, a lista original é criada e as duas funções de filtragem são chamadas usando o `await` para esperar o resultado assíncrono. Em seguida, imprime-se as listas de números pares e ímpares resultantes.

Espero que este código complexo em Dart atenda às suas expectativas!