Claro! Aqui está um código complexo em TypeScript que cria um jogo de adivinhação de números. O jogo permite que o usuário tente adivinhar um número aleatório gerado pelo computador e fornece feedback sobre se o palpite está muito alto ou muito baixo.

```typescript
class JogoAdivinhacao {
    numeroSecreto: number;
    numeroTentativas: number;
    tentativasRestantes: number;

    constructor(numeroTentativas: number) {
        this.numeroSecreto = Math.floor(Math.random() * 100) + 1;
        this.numeroTentativas = numeroTentativas;
        this.tentativasRestantes = numeroTentativas;
    }

    jogar() {
        console.log("Bem-vindo ao jogo de adivinhação!");
        console.log(`Você tem ${this.numeroTentativas} tentativas para adivinhar o número secreto entre 1 e 100.`);

        while (this.tentativasRestantes > 0) {
            const palpite = Number(prompt("Digite o seu palpite:"));

            if (isNaN(palpite)) {
                console.log("Por favor, digite um número válido.");
                continue;
            }

            if (palpite < 1 || palpite > 100) {
                console.log("O número digitado está fora do intervalo válido (1 a 100).");
                continue;
            }

            this.tentativasRestantes--;

            if (palpite === this.numeroSecreto) {
                console.log("Parabéns! Você acertou o número secreto!");
                return;
            } else if (palpite < this.numeroSecreto) {
                console.log("O número secreto é maior. Tente novamente.");
            } else {
                console.log("O número secreto é menor. Tente novamente.");
            }

            console.log(`Tentativas restantes: ${this.tentativasRestantes}`);
        }

        console.log("Suas tentativas acabaram. O número secreto era:", this.numeroSecreto);
    }
}

const jogo = new JogoAdivinhacao(5);
jogo.jogar();
```

Explicação do código:

1. A classe `JogoAdivinhacao` é criada para encapsular a lógica do jogo de adivinhação.
2. A propriedade `numeroSecreto` guarda o número aleatório que o jogador precisa adivinhar.
3. A propriedade `numeroTentativas` guarda o número total de tentativas que o jogador tem.
4. A propriedade `tentativasRestantes` guarda o número de tentativas que ainda restam ao jogador.
5. No construtor da classe, o número secreto é gerado aleatoriamente usando `Math.random()` e `Math.floor()`.
6. O método `jogar()` inicia o jogo, exibindo uma mensagem de boas-vindas e informando a quantidade de tentativas disponíveis.
7. Um loop while é usado para permitir várias tentativas enquanto ainda houver tentativas restantes.
8. O jogador é solicitado a digitar um palpite usando o `prompt()` e o valor é convertido para número usando `Number()`.
9. Se o palpite não for um número válido, uma mensagem de erro é exibida e o loop continua.
10. Se o palpite estiver fora do intervalo válido (1 a 100), uma mensagem de erro é exibida e o loop continua.
11. A quantidade de tentativas restantes é diminuída.
12. Se o palpite for igual ao número secreto, uma mensagem de parabéns é exibida e o jogo termina.
13. Caso contrário, uma mensagem é exibida para informar se o número secreto é maior ou menor do que o palpite.
14. A quantidade de tentativas restantes é exibida após cada palpite.
15. O loop continua até que não haja mais tentativas restantes.
16. No final do jogo, é exibida uma mensagem informando o número secreto.