Claro! Aqui está um exemplo de código complexo em Dart que utiliza conceitos avançados de programação:

```dart
import 'dart:async';

void main() {
  final streamController = StreamController<int>();

  final subscription = streamController.stream
      .where((value) => value % 2 == 0)
      .map((value) => value * 2)
      .listen((value) => print('Valor: $value'));

  for (var i = 1; i <= 10; i++) {
    streamController.sink.add(i);
  }

  streamController.close();
  subscription.cancel();
}
```

Este código cria um `StreamController` que emite números inteiros e, em seguida, cria uma assinatura para escutar o stream. Antes de fechar o stream, o código verifica cada número emitido e, se for par, o dobra e imprime o valor. Por fim, o stream é fechado e a assinatura é cancelada.

Espero que isso seja útil para você! Se precisar de mais exemplos ou tiver alguma dúvida, é só me perguntar!