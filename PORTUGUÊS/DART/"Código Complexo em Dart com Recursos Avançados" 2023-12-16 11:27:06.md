Claro! Aqui está um código complexo em Dart:

```dart
import 'dart:math';

void main() {
  // Criando uma classe complexa
  var minhaClasseComplexa = MinhaClasseComplexa();

  // Chamando um método com parâmetros opcionais e nomeados
  minhaClasseComplexa.metodoComParametrosOpcionaisENomeados(10, parametroNomeado: "Olá");

  // Utilizando uma função de alta ordem
  var resultadoFuncao = minhaClasseComplexa.funcaoDeAltaOrdem((a, b) => a + b, 5, 10);
  print("Resultado da função de alta ordem: $resultadoFuncao");

  // Utilizando uma stream
  var minhaStream = minhaClasseComplexa.criarStream(5);
  minhaStream.listen((valor) {
    print("Valor recebido da stream: $valor");
  });

  // Utilizando uma enumeração
  var diaDaSemana = DiaSemana.sexta;
  print("Dia da semana: $diaDaSemana");
}

class MinhaClasseComplexa {
  // Método com parâmetros opcionais e nomeados
  void metodoComParametrosOpcionaisENomeados(int parametroObrigatorio,
      {int? parametroOpcional, String parametroNomeado = "Padrão"}) {
    print("Parâmetro obrigatório: $parametroObrigatorio");
    print("Parâmetro opcional: $parametroOpcional");
    print("Parâmetro nomeado: $parametroNomeado");
  }

  // Função de alta ordem
  T funcaoDeAltaOrdem<T>(T Function(T, T) funcao, T a, T b) {
    return funcao(a, b);
  }

  // Criação de uma stream
  Stream<int> criarStream(int quantidade) async* {
    for (var i = 0; i < quantidade; i++) {
      await Future.delayed(Duration(seconds: 1));
      yield i;
    }
  }
}

enum DiaSemana { segunda, terca, quarta, quinta, sexta, sabado, domingo }
```

Neste código em Dart, criamos uma classe `MinhaClasseComplexa` com diversos recursos complexos:

1. O método `metodoComParametrosOpcionaisENomeados` recebe um parâmetro obrigatório, um parâmetro opcional e um parâmetro nomeado. Imprimimos os valores recebidos no console.

2. A função de alta ordem `funcaoDeAltaOrdem` recebe uma função e dois parâmetros. Ela executa a função passada como parâmetro e retorna o resultado.

3. A função `criarStream` cria uma stream que emite números de 0 a `quantidade` a cada segundo.

Além disso, utilizamos uma enumeração `DiaSemana` para representar os dias da semana.

Esse código é apenas um exemplo de como pode ser um código complexo em Dart, com o uso de recursos avançados da linguagem. É importante destacar que a complexidade de um código pode variar de acordo com o contexto e os requisitos do projeto.