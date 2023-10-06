Claro! Aqui está um código em Dart que cria uma classe chamada "Funcionario" com propriedades e métodos para representar um funcionário em uma empresa. O código também inclui um exemplo de uso dessa classe.

```dart
class Funcionario {
  String nome;
  int idade;
  String cargo;
  double salario;
  
  Funcionario(this.nome, this.idade, this.cargo, this.salario);
  
  void realizarTarefa() {
    print('$nome está realizando sua tarefa como $cargo.');
  }
  
  void receberSalario() {
    print('$nome recebeu o salário de R\$${salario.toStringAsFixed(2)}.');
  }
}

void main() {
  // Criando instâncias da classe Funcionario
  Funcionario funcionario1 = Funcionario('João', 30, 'Desenvolvedor', 5000.0);
  Funcionario funcionario2 = Funcionario('Maria', 25, 'Designer', 4000.0);
  
  // Acessando propriedades e chamando métodos
  print('Dados do funcionário 1:');
  print('Nome: ${funcionario1.nome}');
  print('Idade: ${funcionario1.idade}');
  print('Cargo: ${funcionario1.cargo}');
  print('Salário: R\$${funcionario1.salario.toStringAsFixed(2)}');
  
  funcionario1.realizarTarefa();
  funcionario1.receberSalario();
  
  print('\nDados do funcionário 2:');
  print('Nome: ${funcionario2.nome}');
  print('Idade: ${funcionario2.idade}');
  print('Cargo: ${funcionario2.cargo}');
  print('Salário: R\$${funcionario2.salario.toStringAsFixed(2)}');
  
  funcionario2.realizarTarefa();
  funcionario2.receberSalario();
}
```

Neste código, declaramos a classe `Funcionario` com as propriedades `nome`, `idade`, `cargo` e `salario`. O construtor da classe é responsável por inicializar essas propriedades quando uma instância da classe é criada.

Dentro da classe, temos dois métodos: `realizarTarefa()` e `receberSalario()`. O método `realizarTarefa()` simplesmente imprime uma mensagem informando que o funcionário está realizando sua tarefa. O método `receberSalario()` imprime uma mensagem informando que o funcionário recebeu seu salário.

No método `main()`, criamos duas instâncias da classe `Funcionario` e atribuímos valores às propriedades de cada funcionário. Em seguida, acessamos as propriedades e chamamos os métodos para cada funcionário, exibindo as informações no console.

Esse código mostra um exemplo simples de como criar e usar uma classe em Dart para representar funcionários de uma empresa. É claro que esse código pode ser expandido e personalizado de acordo com as necessidades específicas de um sistema.