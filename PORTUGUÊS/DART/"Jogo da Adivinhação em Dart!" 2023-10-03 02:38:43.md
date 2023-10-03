Claro! Aqui está um código complexo em Dart para você:

```dart
import 'dart:math';

void main() {
  print("Bem-vindo ao jogo da adivinhação!");
  
  int numeroSecreto = gerarNumeroSecreto();
  int tentativas = 0;
  bool acertou = false;
  
  while (!acertou) {
    tentativas++;
    
    int palpite = receberPalpite(tentativas);
    
    if (palpite == numeroSecreto) {
      acertou = true;
      print("Parabéns! Você acertou o número secreto em $tentativas tentativas.");
    } else if (palpite < numeroSecreto) {
      print("O número secreto é maior que o seu palpite.");
    } else {
      print("O número secreto é menor que o seu palpite.");
    }
  }
}

int gerarNumeroSecreto() {
  Random random = Random();
  return random.nextInt(100) + 1;
}

int receberPalpite(int tentativas) {
  int palpite;
  bool palpiteValido = false;
  
  while (!palpiteValido) {
    stdout.write("Tentativa $tentativas: Digite um número entre 1 e 100: ");
    String input = stdin.readLineSync();
    
    try {
      palpite = int.parse(input);
      
      if (palpite >= 1 && palpite <= 100) {
        palpiteValido = true;
      } else {
        print("O número digitado está fora do intervalo válido. Tente novamente.");
      }
    } catch (exception) {
      print("Entrada inválida. Digite apenas números. Tente novamente.");
    }
  }
  
  return palpite;
}
```

Explicação do código:

1. Importamos a biblioteca `dart:math` para usar a classe `Random`, que nos permite gerar números aleatórios.
2. Na função `main()`, iniciamos o jogo da adivinhação exibindo uma mensagem de boas-vindas.
3. Geramos um número secreto chamando a função `gerarNumeroSecreto()`, que retorna um número inteiro aleatório entre 1 e 100.
4. Inicializamos as variáveis `tentativas` e `acertou` com os valores 0 e `false`, respectivamente. A variável `tentativas` será usada para contar o número de tentativas do jogador e `acertou` indicará se o jogador acertou o número secreto.
5. Iniciamos um loop `while` que continuará até o jogador acertar o número secreto.
6. Dentro do loop, incrementamos o contador de tentativas `tentativas++`.
7. Chamamos a função `receberPalpite()` para obter o palpite do jogador.
8. Verificamos se o palpite do jogador é igual ao número secreto. Se for, atribuímos `true` à variável `acertou` e exibimos uma mensagem de parabéns junto com o número de tentativas.
9. Caso contrário, verificamos se o palpite é menor ou maior que o número secreto e exibimos uma mensagem adequada.
10. Ao final da iteração do loop, ele retornará ao passo 6, continuando até que o jogador acerte o número secreto.
11. A função `gerarNumeroSecreto()` utiliza a classe `Random` para gerar um número aleatório entre 1 e 100.
12. A função `receberPalpite()` é responsável por obter o palpite do jogador. Ela utiliza um loop `while` para garantir que o palpite seja válido, ou seja, um número entre 1 e 100. Ela também trata exceções para casos em que o jogador digita uma entrada inválida.

Espero que este código complexo em Dart atenda às suas expectativas!