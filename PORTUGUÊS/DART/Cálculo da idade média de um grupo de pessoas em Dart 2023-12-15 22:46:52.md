Claro! Aqui está um código complexo em Dart:

```dart
import 'dart:io';

void main() {
  // Criação de uma classe chamada Pessoa
  class Pessoa {
    String nome;
    int idade;
    
    // Construtor da classe Pessoa
    Pessoa({this.nome, this.idade});
    
    // Método para imprimir os detalhes da pessoa
    void imprimirDetalhes() {
      print('Nome: $nome');
      print('Idade: $idade');
    }
  }
  
  // Função para ler os dados de uma pessoa a partir do terminal
  Pessoa lerDadosPessoa() {
    print('Digite o nome da pessoa:');
    String nome = stdin.readLineSync();
    
    print('Digite a idade da pessoa:');
    int idade = int.parse(stdin.readLineSync());
    
    return Pessoa(nome: nome, idade: idade);
  }
  
  // Função para calcular a idade média de um grupo de pessoas
  double calcularIdadeMedia(List<Pessoa> pessoas) {
    int somaIdades = 0;
    
    for (var pessoa in pessoas) {
      somaIdades += pessoa.idade;
    }
    
    double media = somaIdades / pessoas.length;
    return media;
  }
  
  // Criação de uma lista de pessoas
  List<Pessoa> pessoas = [];
  
  // Leitura dos dados de 3 pessoas e armazenamento na lista
  for (var i = 0; i < 3; i++) {
    print('Digite os dados da pessoa ${i+1}:');
    pessoas.add(lerDadosPessoa());
  }
  
  // Impressão dos detalhes de cada pessoa
  for (var pessoa in pessoas) {
    pessoa.imprimirDetalhes();
    print('---');
  }
  
  // Cálculo da idade média e impressão do resultado
  double idadeMedia = calcularIdadeMedia(pessoas);
  print('A idade média do grupo é: $idadeMedia');
}
```

Neste código em Dart, temos a definição de uma classe chamada Pessoa, que possui atributos de nome e idade, um construtor e um método para imprimir os detalhes da pessoa. 

Além disso, temos uma função chamada lerDadosPessoa, que permite a leitura dos dados de uma pessoa a partir do terminal. Esses dados são utilizados para criar objetos da classe Pessoa, que são armazenados em uma lista chamada pessoas.

Temos também uma função chamada calcularIdadeMedia, que recebe a lista de pessoas e calcula a idade média do grupo.

No código principal, são lidos os dados de 3 pessoas e armazenados na lista. Em seguida, os detalhes de cada pessoa são impressos na tela. Por fim, é calculada a idade média do grupo e o resultado é exibido.

Esse código é apenas um exemplo complexo e diferenciado, mas pode ser adaptado e modificado de acordo com as necessidades específicas de cada projeto.