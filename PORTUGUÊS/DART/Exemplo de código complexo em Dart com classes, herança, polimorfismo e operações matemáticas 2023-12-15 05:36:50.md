Claro! Aqui está um exemplo de um código complexo em Dart que utiliza classes, herança, polimorfismo e operações matemáticas:

```dart
import 'dart:math';

// Classe abstrata para representar um animal
abstract class Animal {
  String nome;
  int idade;
  
  Animal(this.nome, this.idade);
  
  void fazerBarulho();
}

// Classe que representa um cão, que é um animal
class Cao extends Animal {
  bool lateAlto;
  
  Cao(String nome, int idade, this.lateAlto) : super(nome, idade);
  
  @override
  void fazerBarulho() {
    if (lateAlto) {
      print('O cão $nome está latindo alto!');
    } else {
      print('O cão $nome está latindo baixo.');
    }
  }
}

// Classe que representa um gato, que é um animal
class Gato extends Animal {
  bool miaAlto;
  
  Gato(String nome, int idade, this.miaAlto) : super(nome, idade);
  
  @override
  void fazerBarulho() {
    if (miaAlto) {
      print('O gato $nome está miando alto!');
    } else {
      print('O gato $nome está miando baixo.');
    }
  }
}

// Função principal
void main() {
  // Criando uma lista de animais
  List<Animal> animais = [
    Cao('Rex', 5, true),
    Gato('Felix', 3, false),
  ];
  
  // Iterando pela lista de animais
  for (var animal in animais) {
    print('Nome: ${animal.nome}');
    print('Idade: ${animal.idade}');
    animal.fazerBarulho();
    
    // Gerando um número aleatório entre 1 e 10
    var numeroAleatorio = Random().nextInt(10) + 1;
    print('Número aleatório: $numeroAleatorio');
    
    // Realizando algumas operações matemáticas
    var soma = numeroAleatorio + animal.idade;
    var subtracao = numeroAleatorio - animal.idade;
    var multiplicacao = numeroAleatorio * animal.idade;
    var divisao = numeroAleatorio / animal.idade;
    
    print('Soma: $soma');
    print('Subtração: $subtracao');
    print('Multiplicação: $multiplicacao');
    print('Divisão: $divisao');
    
    print('------------------------');
  }
}
```

Neste exemplo, temos três classes: `Animal` (classe abstrata), `Cao` (subclasse de `Animal`) e `Gato` (subclasse de `Animal`). Cada uma das classes representa um tipo de animal, com atributos específicos. A classe `Animal` possui um construtor e um método abstrato `fazerBarulho`, que é implementado nas subclasses.

Na função `main`, criamos uma lista de animais contendo um objeto `Cao` e um objeto `Gato`. Em seguida, iteramos pela lista, exibindo informações sobre cada animal e chamando o método `fazerBarulho`.

Além disso, geramos um número aleatório usando a classe `Random` do pacote `dart:math` e realizamos algumas operações matemáticas simples utilizando esse número e a idade do animal.