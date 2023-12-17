Claro! Aqui está um exemplo de um código complexo em Dart, que utiliza conceitos avançados e é explicado em detalhes:

```dart
import 'dart:async';

// Classe base para representar um animal
abstract class Animal {
  String nome;
  int idade;

  Animal(this.nome, this.idade);

  // Método abstrato para emitir som
  void emitirSom();
}

// Classe que representa um cachorro
class Cachorro extends Animal {
  Cachorro(String nome, int idade) : super(nome, idade);

  @override
  void emitirSom() {
    print('$nome: Au Au!');
  }

  // Método específico para cachorros
  void abanarRabo() {
    print('$nome está abanando o rabo!');
  }
}

// Classe que representa um gato
class Gato extends Animal {
  Gato(String nome, int idade) : super(nome, idade);

  @override
  void emitirSom() {
    print('$nome: Miau!');
  }

  // Método específico para gatos
  void arranharMoveis() {
    print('$nome está arranhando os móveis!');
  }
}

// Função principal
void main() {
  // Instanciando um cachorro e um gato
  var cachorro = Cachorro('Rex', 3);
  var gato = Gato('Felix', 2);

  // Emitindo sons dos animais
  cachorro.emitirSom();
  gato.emitirSom();

  // Chamando métodos específicos dos animais
  cachorro.abanarRabo();
  gato.arranharMoveis();

  // Demonstrando uso de Stream para contar o tempo
  var contador = Stream<int>.periodic(Duration(seconds: 1), (segundo) => segundo + 1)
      .take(5)
      .asBroadcastStream();

  contador.listen((segundo) {
    print('Já se passaram $segundo segundo(s)!');
  });
}
```

Neste código em Dart, temos a definição de três classes: `Animal`, `Cachorro` e `Gato`. A classe `Animal` é abstrata e possui propriedades comuns a todos os animais, como o nome e a idade, além de um método abstrato `emitirSom()`. As classes `Cachorro` e `Gato` herdam da classe `Animal` e implementam o método `emitirSom()` de forma específica para cada animal.

No `main()`, criamos instâncias de um `Cachorro` e um `Gato`, e chamamos o método `emitirSom()` de cada um, exibindo os sons emitidos por eles. Além disso, chamamos métodos específicos de cada animal, como `abanarRabo()` para o cachorro e `arranharMoveis()` para o gato.

Também demonstramos o uso de `Stream` para contar o tempo, criando um contador que emite um número a cada segundo durante 5 segundos. O método `listen()` é utilizado para escutar os eventos emitidos pelo `Stream` e exibir a mensagem correspondente.

Espero que este exemplo atenda às suas expectativas!