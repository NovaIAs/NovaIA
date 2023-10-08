Claro! Aqui está um código complexo em Dart que utiliza conceitos avançados de programação assíncrona e orientação a objetos:

```dart
import 'dart:async';

class Pessoa {
  String nome;
  int idade;

  Pessoa(this.nome, this.idade);

  void cumprimentar() {
    print("Olá, meu nome é $nome e eu tenho $idade anos.");
  }
}

Future<void> main() async {
  var pessoas = <Pessoa>[
    Pessoa("João", 25),
    Pessoa("Maria", 30),
    Pessoa("Carlos", 40),
  ];

  var futures = pessoas.map((pessoa) => esperarCumprimentar(pessoa)).toList();

  await Future.wait(futures);
}

Future<void> esperarCumprimentar(Pessoa pessoa) async {
  await Future.delayed(Duration(seconds: 2));
  pessoa.cumprimentar();
}
```

Este código cria uma classe chamada `Pessoa` que possui propriedades de nome e idade, bem como um método `cumprimentar()` que imprime uma mensagem de cumprimento com o nome e idade da pessoa.

Em seguida, o código principal (`main()`) cria uma lista de objetos `Pessoa` e, em seguida, mapeia cada pessoa para uma função assíncrona chamada `esperarCumprimentar()`. Essa função espera um atraso de 2 segundos usando `Future.delayed()` e, em seguida, chama o método `cumprimentar()` na pessoa.

A função `main()` usa `Future.wait()` para aguardar a conclusão de todas as funções assíncronas. Isso permite que todas as pessoas cumprimentem ao mesmo tempo, com um atraso de 2 segundos entre cada cumprimento.

Ao executar este código, você verá as mensagens de cumprimento impressas para cada pessoa, iniciando após um atraso de 2 segundos.

Espero que este código complexo em Dart atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, fique à vontade para perguntar.