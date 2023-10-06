Claro! Aqui está um código complexo em Dart que usa conceitos avançados de programação assíncrona e orientação a objetos:

```dart
import 'dart:async';

class Pessoa {
  String nome;
  
  Pessoa(this.nome);
  
  Future<String> cumprimentar() async {
    await Future.delayed(Duration(seconds: 2)); // Simula um atraso de 2 segundos
    
    return "Olá, $nome!";
  }
}

void main() {
  final pessoa = Pessoa("João");
  
  pessoa.cumprimentar().then((mensagem) {
    print(mensagem);
    final idade = calcularIdade();
    print("Você tem $idade anos de idade.");
  }).catchError((erro) {
    print("Ops, ocorreu um erro: $erro");
  });
}

Future<int> calcularIdade() async {
  await Future.delayed(Duration(seconds: 1)); // Simula um atraso de 1 segundo
  
  final idade = 30;
  return idade;
}
```

Este código é um exemplo de como utilizar a programação assíncrona em Dart. Ele cria uma classe `Pessoa` com um método `cumprimentar()`, que retorna uma `Future` de uma string com uma mensagem de cumprimento personalizada. A função `main()` cria uma instância de `Pessoa` chamada "João" e chama o método `cumprimentar()`. 

A chamada do método `cumprimentar()` é feita de forma assíncrona usando o operador `await`, permitindo que o programa continue executando outras tarefas enquanto aguarda a conclusão do método assíncrono. Após o cumprimento, o código chama a função `calcularIdade()` para obter a idade da pessoa assincronamente.

A função `calcularIdade()` também retorna uma `Future`, que é resolvida após um atraso simulado de 1 segundo. Em seguida, o valor da idade é retornado.

No final, o código imprime a mensagem de cumprimento e a idade da pessoa, ou exibe uma mensagem de erro caso ocorra algum problema durante a execução assíncrona.

Espero que este exemplo tenha sido útil para demonstrar conceitos complexos em Dart!