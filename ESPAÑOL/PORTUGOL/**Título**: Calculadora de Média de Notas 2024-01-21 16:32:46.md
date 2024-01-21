```portuguol
programa CalculaMediaNotas {

    // Declaração das variáveis
    real notas[10], media;
    inteiro i;

    // Solicita ao usuário o input das notas
    para i de 1 até 10 faça
        escreva("Digite a nota do aluno ", i, ": ");
        leia(notas[i]);
    fim para

    // Calcula a média das notas
    media := 0;
    para i de 1 até 10 faça
        media := media + notas[i];
    fim para
    media := media / 10;

    // Imprime a média das notas na tela
    escreva("A média das notas é: ", media);

}
```

Explicação do código:

* O código solicita ao usuário o input de 10 notas.
* Em seguida, calcula a média das notas somando todas as notas e dividindo por 10.
* Finalmente, imprime a média das notas na tela.