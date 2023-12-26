Claro! Aqui está um exemplo de código complexo em Dart que utiliza conceitos avançados da linguagem:

```dart
import 'dart:async';

void main() {
  final streamController = StreamController<int>();
  final streamTransformer = StreamTransformer<int, String>.fromHandlers(
    handleData: (value, sink) {
      if (value % 2 == 0) {
        sink.add('Par: $value');
      } else {
        sink.add('Ímpar: $value');
      }
    },
    handleError: (error, stackTrace, sink) {
      sink.addError('Ocorreu um erro: $error');
    },
    handleDone: (sink) {
      sink.close();
    },
  );

  final subscription = streamController.stream
      .transform(streamTransformer)
      .listen(
        (value) => print(value),
        onError: (error) => print(error),
        onDone: () => print('Stream finalizada'),
      );

  for (int i = 1; i <= 10; i++) {
    streamController.sink.add(i);
  }

  streamController.sink.close();
}
```

Este código mostra um exemplo de uso de streams, transformações e gerenciamento de erros em Dart.

Primeiro, importamos o pacote `dart:async`, que fornece as classes necessárias para trabalhar com streams.

Em seguida, criamos um `StreamController<int>` chamado `streamController`, que será responsável por emitir os números inteiros.

Definimos também um `StreamTransformer<int, String>`, chamado `streamTransformer`, que recebe um número inteiro e o transforma em uma String indicando se é par ou ímpar. O transformador também lida com erros e notifica quando a stream é finalizada.

Depois, criamos uma assinatura (`subscription`) para escutar os eventos da stream. Utilizamos o método `listen` para especificar as ações a serem executadas quando um valor é emitido, quando ocorre um erro ou quando a stream é finalizada.

Em seguida, usamos um loop `for` para adicionar os números de 1 a 10 à stream usando `streamController.sink.add(i)`.

Finalmente, fechamos o sink da stream usando `streamController.sink.close()`.

Espero que este exemplo te ajude a entender melhor o uso de streams e transformações em Dart!