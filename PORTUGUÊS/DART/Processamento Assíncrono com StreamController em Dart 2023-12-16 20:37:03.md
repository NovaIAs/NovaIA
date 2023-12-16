Claro! Aqui está um código complexo em Dart que utiliza conceitos avançados e é explicado em detalhes:

```dart
import 'dart:async';

void main() {
  final streamController = StreamController<int>(); // Cria um StreamController para manipular a transmissão de dados assíncrona
  
  final subscription = streamController.stream // Obtém a stream do StreamController
      .where((value) => value % 2 == 0) // Filtra apenas os números pares
      .map((value) => value * 2) // Multiplica cada número par por 2
      .listen((value) => print('Número par multiplicado por 2: $value')); // Escuta os eventos da stream e imprime o resultado
  
  for (var i = 1; i <= 10; i++) {
    streamController.sink.add(i); // Adiciona os números de 1 a 10 à stream
  }
  
  streamController.close(); // Fecha a stream após o loop
  
  subscription.onDone(() => print('Processamento concluído.')); // Imprime uma mensagem quando o processamento da stream for concluído
}
```

Nesse código, estamos utilizando a linguagem Dart para criar um StreamController, que é uma forma de transmitir dados assincronamente. 

Primeiro, importamos a biblioteca `dart:async`, que fornece classes para trabalhar com programação assíncrona. 

Em seguida, no método `main`, criamos um `StreamController` chamado `streamController`. O `StreamController` é responsável por gerar uma `stream`, que é uma sequência de eventos assíncronos. 

Utilizamos o método `stream` para obter a `stream` do `StreamController`. Em seguida, encadeamos uma série de operações na `stream` utilizando os métodos `where`, `map` e `listen`. 

O método `where` filtra apenas os números pares, utilizando uma função anônima que verifica se o número é divisível por 2. 

O método `map` multiplica cada número par por 2, utilizando uma função anônima que realiza o cálculo. 

Finalmente, o método `listen` escuta os eventos da `stream` e imprime o resultado utilizando uma função anônima que recebe cada valor emitido pela `stream`.

Em um loop de 1 a 10, adicionamos os números à `stream` utilizando o método `sink.add`. Após o loop, fechamos a `stream` utilizando o método `close`.

Por fim, utilizamos o método `onDone` para imprimir uma mensagem quando o processamento da `stream` for concluído.

Esse código demonstra o uso avançado de `stream` e `StreamController` em Dart, permitindo a manipulação de eventos assíncronos de forma elegante e eficiente.