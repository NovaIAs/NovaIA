Claro! Aqui está um exemplo de um código complexo em Dart:

```dart
import 'dart:async';
import 'dart:math';

void main() {
  final random = Random();
  
  print('Bem-vindo(a) ao jogo de adivinhação!');
  print('Vou pensar em um número entre 1 e 100. Tente adivinhar qual é!');
  
  final answer = random.nextInt(100) + 1;
  int attempts = 0;
  
  final streamController = StreamController<int>();
  
  final subscription = streamController.stream.listen((guess) {
    attempts++;
    
    if (guess == answer) {
      print('Parabéns! Você acertou em $attempts tentativas!');
      streamController.close();
    } else if (guess < answer) {
      print('Tente um número maior!');
    } else {
      print('Tente um número menor!');
    }
  });
  
  streamController.sink.add(50);
  streamController.sink.add(25);
  streamController.sink.add(75);
  streamController.sink.add(10);
  streamController.sink.add(90);
  
  subscription.onDone(() {
    print('O jogo acabou! O número correto era $answer.');
  });
  
  streamController.close();
}
```

Neste código, estamos criando um jogo de adivinhação em que o programa escolhe um número aleatório entre 1 e 100 e o jogador deve tentar adivinhar qual é. 

Primeiro, importamos as bibliotecas necessárias: `dart:async` para lidar com streams e `dart:math` para gerar números aleatórios.

Em seguida, exibimos uma mensagem de boas-vindas e explicamos as regras do jogo.

Depois, geramos um número aleatório utilizando `random.nextInt(100) + 1`, que retorna um número inteiro entre 1 e 100.

Em seguida, inicializamos a variável `attempts` para contar o número de tentativas.

Criamos um `StreamController` chamado `streamController` para receber os palpites do jogador.

Em seguida, utilizamos a função `listen` para se inscrever nas atualizações do stream. Dentro do `listen`, verificamos se o palpite do jogador é igual ao número correto. Se for, exibimos uma mensagem de parabéns e fechamos o stream. Caso contrário, informamos ao jogador se o palpite foi muito alto ou muito baixo.

Em seguida, adicionamos alguns palpites ao stream utilizando `streamController.sink.add()`.

Por fim, utilizamos o método `onDone` para executar uma ação quando o stream é fechado. Neste caso, exibimos a mensagem final informando qual era o número correto.

Espero que este exemplo possa lhe ajudar a entender melhor a codificação em Dart!