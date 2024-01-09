```portugol
program fatorial
    var n, fat;
    inicio
        escreva("Digite um número inteiro positivo: ");
        leia(n);
        fat := 1;
        para i de 1 até n faça
            fat := fat * i;
        fimpara;
        escreva("Fatorial de ", n, " é ", fat);
    fim

program primo
    var n, i, flag;
    inicio
        escreva("Digite um número inteiro positivo: ");
        leia(n);
        flag := 1;
        para i de 2 até n - 1 faça
            se n mod i = 0 então
                flag := 0;
                saia;
            fimse;
        fimpara;
        se flag = 1 então
            escreva(n, " é primo");
        senão
            escreva(n, " não é primo");
        fimse;
    fim

program fibonacci
    var n, a, b, c;
    inicio
        escreva("Digite um número inteiro positivo: ");
        leia(n);
        a := 0;
        b := 1;
        para i de 1 até n faça
            c := a + b;
            a := b;
            b := c;
        fimpara;
        escreva("O ", n, "-ésimo número da série de Fibonacci é ", c);
    fim

program maior_menor
    var n, maior, menor;
    inicio
        escreva("Digite um número inteiro positivo: ");
        leia(n);
        maior := n;
        menor := n;
        para i de 2 até n faça
            escreva("Digite um número inteiro positivo: ");
            leia(n);
            se n > maior então
                maior := n;
            fimse;
            se n < menor então
                menor := n;
            fimse;
        fimpara;
        escreva("Maior: ", maior);
        escreva("Menor: ", menor);
    fim

program media_aritmetica
    var n, soma, media;
    inicio
        escreva("Digite um número inteiro positivo: ");
        leia(n);
        soma := 0;
        para i de 1 até n faça
            escreva("Digite um número inteiro positivo: ");
            leia(x);
            soma := soma + x;
        fimpara;
        media := soma / n;
        escreva("Média aritmética: ", media);
    fim
```