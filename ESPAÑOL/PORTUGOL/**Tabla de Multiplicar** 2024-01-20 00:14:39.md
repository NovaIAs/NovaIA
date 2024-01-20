```portugol

Inicio

Procedimento ImprimeCabecalho;
Procedimento ImprimeLinha;

ImprimeCabecalho();

Var
   i, j, k, l: Inteiro;
   Texto: Vetor de Caracteres;
   Matriz: Matriz[10,10] de Inteiro;
   Ponteiro: Ponteiro;

Para i de 1 ate 10 Faça
   Para j de 1 ate 10 Faça
      Matriz[i,j] <- i * j;
   Fim Para
Fim Para

Para k de 1 ate 10 Faça
   Para l de 1 ate 10 Faça
      ImprimeLinha(Matriz[k,l]);
   Fim Para
Fim Para

Ponteiro <- &Texto;

Para i de 1 ate 10 Faça
   Para j de 1 ate 10 Faça
      Ponteiro['A' + (i - 1)][j - 1] <- '*' + (Matriz[i,j] Mod 26);
   Fim Para
Fim Para

Para i de 1 ate 10 Faça
   Texto[i] <- '|';
   Para j de 1 ate 10 Faça
      Texto[j + 10] <- Ponteiro['A' + (i - 1)][j - 1];
   Fim Para
Fim Para

ImprimeCabecalho();

Para i de 1 ate 10 Faça
   EscrevaTexto(Texto);
Fim Para

ImprimeLinha();

Fim

Procedimento ImprimeCabecalho;
   EscrevaTexto("     1   2   3   4   5   6   7   8   9   10");
   EscrevaTexto("   +---+---+---+---+---+---+---+---+---+---+");
Fim Procedimento

Procedimento ImprimeLinha;
   EscrevaTexto("   |---|---|---|---|---|---|---|---|---|---|");
Fim Procedimento

```

Este programa imprime uma tabela 10x10 com os produtos dos números de 1 a 10. Ele também imprime um cabeçalho e uma linha para separar a tabela. O programa usa um procedimento para imprimir o cabeçalho e outro procedimento para imprimir a linha.

O programa usa uma matriz para armazenar os produtos dos números de 1 a 10. A matriz é declarada como uma matriz de 10 linhas por 10 colunas de inteiros. O programa usa dois laços para percorrer a matriz e armazenar os produtos dos números de 1 a 10.

O programa usa um ponteiro para armazenar os caracteres que serão impressos na tabela. O ponteiro é declarado como um ponteiro para um vetor de caracteres. O programa usa dois laços para percorrer a matriz e armazenar os caracteres que serão impressos na tabela.

O programa usa um procedimento para imprimir a tabela. O procedimento usa dois laços para percorrer a matriz e imprimir os caracteres que serão impressos na tabela. O procedimento também usa dois procedimentos para imprimir o cabeçalho e a linha.

O programa usa um procedimento para imprimir o cabeçalho. O procedimento imprime o texto "     1   2   3   4   5   6   7   8   9   10".

O programa usa um procedimento para imprimir a linha. O procedimento imprime o texto "   +---+---+---+---+---+---+---+---+---+---+".