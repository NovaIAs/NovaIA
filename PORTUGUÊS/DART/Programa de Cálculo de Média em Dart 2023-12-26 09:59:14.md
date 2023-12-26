Claro! Aqui está um exemplo de um código complexo em Dart:

```dart
import 'dart:io';

void main() {
  print("Bem-vindo ao programa de cálculo de média!");

  List<double> notas = [];

  for (int i = 1; i <= 4; i++) {
    double nota = obterNota(i);
    notas.add(nota);
  }

  double media = calcularMedia(notas);
  String situacao = obterSituacao(media);

  imprimirResultado(media, situacao);
}

double obterNota(int numeroNota) {
  double nota;

  while (true) {
    stdout.write("Digite a nota $numeroNota: ");
    String input = stdin.readLineSync();

    try {
      nota = double.parse(input);
      if (nota >= 0 && nota <= 10) {
        break;
      } else {
        print("A nota deve estar entre 0 e 10. Tente novamente.");
      }
    } catch (e) {
      print("Valor inválido. Tente novamente.");
    }
  }

  return nota;
}

double calcularMedia(List<double> notas) {
  double soma = 0;

  for (double nota in notas) {
    soma += nota;
  }

  double media = soma / notas.length;
  return media;
}

String obterSituacao(double media) {
  if (media >= 7) {
    return "Aprovado";
  } else if (media >= 5) {
    return "Recuperação";
  } else {
    return "Reprovado";
  }
}

void imprimirResultado(double media, String situacao) {
  print("\n=== Resultado ===");
  print("Média: $media");
  print("Situação: $situacao");
}
```

Este código é um programa simples de cálculo de média em Dart. Ele solicita ao usuário que digite quatro notas e, em seguida, calcula a média das notas e determina a situação do aluno (aprovado, recuperação ou reprovado).

O programa começa exibindo uma mensagem de boas-vindas. Em seguida, utiliza um loop para solicitar que o usuário digite cada uma das quatro notas. Para cada nota digitada, o programa verifica se está dentro do intervalo válido (0 a 10) e, caso não esteja, exibe uma mensagem de erro e solicita novamente a nota.

Após obter todas as notas, o programa chama a função `calcularMedia` para calcular a média das notas. Em seguida, chama a função `obterSituacao` para determinar a situação do aluno com base na média calculada.

Por fim, o programa chama a função `imprimirResultado` para exibir a média e a situação do aluno na saída.

Espero que este exemplo tenha sido útil para você! Se tiver alguma dúvida, estou à disposição para ajudar.