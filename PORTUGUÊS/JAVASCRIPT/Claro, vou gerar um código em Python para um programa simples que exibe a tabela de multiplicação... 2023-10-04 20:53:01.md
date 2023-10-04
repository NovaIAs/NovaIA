Claro, vou criar um código complexo em JavaScript que simula um jogo de adivinhação. Neste jogo, o computador escolhe um número aleatório e o jogador tem que tentar adivinhar qual é esse número. O código incluirá recursos como pontuação, limite de tentativas e uma interface interativa.

```javascript
// Definindo as variáveis necessárias
let numeroAleatorio, palpite, tentativas, acertou;

// Função para gerar um número aleatório entre 1 e 100
function gerarNumeroAleatorio() {
  return Math.floor(Math.random() * 100) + 1;
}

// Função para obter o palpite do jogador
function obterPalpite() {
  return parseInt(prompt("Digite um número entre 1 e 100:"));
}

// Função principal do jogo
function jogarAdivinhacao() {
  numeroAleatorio = gerarNumeroAleatorio();
  tentativas = 0;
  acertou = false;

  while (!acertou) {
    palpite = obterPalpite();
    tentativas++;

    if (palpite === numeroAleatorio) {
      acertou = true;
      alert("Parabéns! Você acertou o número em " + tentativas + " tentativas.");
    } else if (palpite < numeroAleatorio) {
      alert("Tente um número maior.");
    } else {
      alert("Tente um número menor.");
    }

    // Verificar se o jogador atingiu o limite de tentativas
    if (tentativas === 10 && !acertou) {
      alert("Suas tentativas acabaram. O número era " + numeroAleatorio + ".");
      break;
    }
  }
}

// Chamando a função do jogo
jogarAdivinhacao();
```

Agora, vamos entender o código:

1. A variável `numeroAleatorio` será usada para armazenar o número escolhido pelo computador.

2. A função `gerarNumeroAleatorio` é responsável por gerar um número aleatório entre 1 e 100 usando o método `Math.random()`.

3. A função `obterPalpite` será usada para obter o palpite do jogador através de um prompt.

4. A função principal `jogarAdivinhacao` é onde o jogo acontece. Ela inicia definindo o número aleatório, as tentativas e a variável `acertou` como `false`.

5. Em um loop while, o jogador continuará tentando adivinhar o número até que `acertou` seja verdadeiro.

6. Dentro do loop, o palpite do jogador é obtido e o número de tentativas é incrementado.

7. Se o palpite do jogador for igual ao número aleatório, o jogador acertou e é exibida uma mensagem de parabéns.

8. Se o palpite for menor que o número aleatório, é exibida uma mensagem para tentar um número maior.

9. Se o palpite for maior que o número aleatório, é exibida uma mensagem para tentar um número menor.

10. Após cada palpite, o código verifica se o jogador atingiu o limite de tentativas (10) e, se não acertou, exibe uma mensagem informando qual era o número.

11. Por fim, a função `jogarAdivinhacao` é chamada para iniciar o jogo.

Espero que este código complexo em JavaScript atenda às suas expectativas.