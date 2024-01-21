```porturgol
program jogo_de_adivinhacao;

var
    num_secreto: inteiro;
    chute: inteiro;
    tentativas: inteiro;

inicio
    // Gera um número secreto aleatório entre 1 e 100
    num_secreto := random(1, 100);

    // Define o número de tentativas para 10
    tentativas := 10;

    // Enquanto o número de tentativas for maior que 0 e o chute for diferente do número secreto, repete o loop
    enquanto tentativas > 0 e chute <> num_secreto faca
        // Solicita ao jogador que faça um chute
        escreva("Adivinhe o número secreto (entre 1 e 100): ");
        leia(chute);

        // Decrementa o número de tentativas
        tentativas := tentativas - 1;

        // Verifica se o chute é maior, menor ou igual ao número secreto
        se chute > num_secreto entao
            escreva("O seu chute é maior que o número secreto.\n");
        senao se chute < num_secreto entao
            escreva("O seu chute é menor que o número secreto.\n");
        senao
            escreva("Parabéns! Você acertou o número secreto.\n");
        fim_se
    fim_enquanto

    // Verifica se o jogador perdeu o jogo
    se tentativas = 0 entao
        escreva("Você perdeu o jogo. O número secreto era ", num_secreto, ".\n");
    fim_se
fim_programa
```

Explicação do código:

* O programa começa definindo o número secreto aleatoriamente entre 1 e 100 usando a função `random`.
* Em seguida, define o número de tentativas para 10.
* O programa então entra em um loop `enquanto` que se repete enquanto o número de tentativas for maior que 0 e o chute for diferente do número secreto.
* Dentro do loop, o programa solicita ao jogador que faça um chute e decrementa o número de tentativas.
* O programa então verifica se o chute é maior, menor ou igual ao número secreto e imprime uma mensagem correspondente.
* O loop continua até que o jogador acerte o número secreto ou perca o jogo (tentativas = 0).
* Se o jogador perder o jogo, o programa imprime o número secreto.