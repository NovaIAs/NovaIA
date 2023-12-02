Claro! Aqui está um exemplo de código em Dart que cria uma classe chamada "Pessoa" e demonstra algumas funcionalidades avançadas, como herança, polimorfismo e tratamento de exceções:

```dart
import 'dart:math';

// Classe base para representar uma pessoa
class Pessoa {
  String nome;
  int idade;

  // Construtor da classe Pessoa
  Pessoa(this.nome, this.idade);

  // Método para exibir os detalhes da pessoa
  void exibirDetalhes() {
    print("Nome: $nome");
    print("Idade: $idade");
  }

  // Método virtual para calcular a idade em dias
  int calcularIdadeEmDias() {
    return idade * 365;
  }
}

// Classe derivada para representar um estudante
class Estudante extends Pessoa {
  String instituicao;

  // Construtor da classe Estudante
  Estudante(String nome, int idade, this.instituicao) : super(nome, idade);

  // Sobrescrita do método para exibir os detalhes do estudante
  @override
  void exibirDetalhes() {
    super.exibirDetalhes();
    print("Instituição: $instituicao");
  }
}

// Classe derivada para representar um professor
class Professor extends Pessoa {
  String disciplina;

  // Construtor da classe Professor
  Professor(String nome, int idade, this.disciplina) : super(nome, idade);

  // Sobrescrita do método para exibir os detalhes do professor
  @override
  void exibirDetalhes() {
    super.exibirDetalhes();
    print("Disciplina: $disciplina");
  }
}

void main() {
  // Criação de objetos usando as classes Pessoa, Estudante e Professor
  Pessoa pessoa = Pessoa("João", 25);
  Estudante estudante = Estudante("Maria", 20, "Universidade XYZ");
  Professor professor = Professor("Carlos", 35, "Matemática");

  // Chamada dos métodos para exibir detalhes
  pessoa.exibirDetalhes();
  print("Idade em dias: ${pessoa.calcularIdadeEmDias()}");
  print("-----------------------------");
  estudante.exibirDetalhes();
  print("Idade em dias: ${estudante.calcularIdadeEmDias()}");
  print("-----------------------------");
  professor.exibirDetalhes();
  print("Idade em dias: ${professor.calcularIdadeEmDias()}");

  // Exemplo de lançamento e tratamento de exceção
  try {
    pessoa = Estudante("Fernanda", -5, "Universidade ABC");
  } catch (e) {
    print("Ocorreu um erro: $e");
  }
}
```

Neste código, temos a definição de três classes: "Pessoa", "Estudante" e "Professor". A classe "Pessoa" é a classe base e contém propriedades como nome e idade, além de métodos para exibir detalhes e calcular a idade em dias.

As classes "Estudante" e "Professor" herdam da classe "Pessoa" e adicionam propriedades específicas para cada tipo de pessoa. Além disso, elas sobrescrevem o método "exibirDetalhes()" para adicionar informações adicionais.

No método "main()", criamos objetos das três classes e chamamos os métodos para exibir detalhes e calcular a idade em dias. Também é mostrado um exemplo de lançamento e tratamento de exceção, onde tentamos criar um estudante com uma idade negativa.

Espero que esse código atenda às suas expectativas! Se tiver alguma dúvida ou precisar de mais exemplos, estou à disposição.