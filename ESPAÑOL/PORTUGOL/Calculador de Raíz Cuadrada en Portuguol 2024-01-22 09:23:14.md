```portuguol
programa Calcula_Raiz_Quadrada {

    função Real Raiz_Quadrada(Real x) {
        Real raiz = 0;
        Enquanto (raiz * raiz) < x faça
            raiz := raiz + 0.001;
        fim_enquanto;
        retorna raiz;
    }

    função Principal() {
        Real x, raiz;

        escreva("Digite um número real: ");
        leia(x);

        se (x < 0) então
            escreva("O número digitado é negativo. Não é possível calcular a raiz quadrada de um número negativo.");
        senão
            raiz := Raiz_Quadrada(x);
            escreva("A raiz quadrada de ", x, " é ", raiz);
        fim_se;
    }
}
```

Explicação do código:

O programa Calcula_Raiz_Quadrada calcula a raiz quadrada de um número real positivo. Ele usa uma função recursiva chamada Raiz_Quadrada() para encontrar a raiz quadrada. A função Raiz_Quadrada() funciona incrementando um valor inicial de 0,001 até que o quadrado desse valor seja maior ou igual ao número cuja raiz quadrada está sendo calculada. Quando isso acontece, o valor da raiz quadrada é retornado.

A função Principal() é a função principal do programa. Ela solicita ao usuário que digite um número real e, em seguida, chama a função Raiz_Quadrada() para calcular a raiz quadrada desse número. Se o número digitado for negativo, o programa exibe uma mensagem de erro e encerra. Se o número for positivo, o programa exibe a raiz quadrada do número.