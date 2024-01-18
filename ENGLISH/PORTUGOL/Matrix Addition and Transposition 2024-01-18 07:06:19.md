```portugol
PROGRAMA "Ordenação Matrizes";

VAR
  M1: MATRIZ[1..5, 1..4] DE INTEIRO;  // Matriz 1
  M2: MATRIZ[1..4, 1..5] DE INTEIRO;  // Matriz 2
  M3: MATRIZ[1..5, 1..5] DE INTEIRO;  // Matriz 3
  IAUX, JAUX, KAUX: INTEIRO;         // Variáveis auxiliares

INICIO

  // Lê a Matriz 1
  PARA IAUX := 1 ATE 5 FAÇA
    PARA JAUX := 1 ATE 4 FAÇA
      ESCREVA("Digite o valor da posição (", IAUX, ", ", JAUX, ") da Matriz 1: ");
      LEIA(M1[IAUX, JAUX]);
    FIM PARA;
  FIM PARA;

  // Lê a Matriz 2
  PARA IAUX := 1 ATE 4 FAÇA
    PARA JAUX := 1 ATE 5 FAÇA
      ESCREVA("Digite o valor da posição (", IAUX, ", ", JAUX, ") da Matriz 2: ");
      LEIA(M2[IAUX, JAUX]);
    FIM PARA;
  FIM PARA;

  // Inicializa a Matriz 3 com zeros
  PARA IAUX := 1 ATE 5 FAÇA
    PARA JAUX := 1 ATE 5 FAÇA
      M3[IAUX, JAUX] := 0;
    FIM PARA;
  FIM PARA;

  // Calcula a Matriz 3 como a transposta da Matriz 1
  PARA IAUX := 1 ATE 5 FAÇA
    PARA JAUX := 1 ATE 4 FAÇA
      M3[JAUX, IAUX] := M1[IAUX, JAUX];
    FIM PARA;
  FIM PARA;

  // Calcula a Matriz 3 como a soma da Matriz 3 com a Matriz 2
  PARA IAUX := 1 ATE 5 FAÇA
    PARA JAUX := 1 ATE 5 FAÇA
      M3[IAUX, JAUX] := M3[IAUX, JAUX] + M2[IAUX, JAUX];
    FIM PARA;
  FIM PARA;

  // Imprime a Matriz 3
  ESCREVALN("Matriz 3:");
  PARA IAUX := 1 ATE 5 FAÇA
    PARA JAUX := 1 ATE 5 FAÇA
      ESCREVA(M3[IAUX, JAUX], " ");
    FIM PARA;
    ESCREVALN;
  FIM PARA;

FIM PROGRAMA;
```

Explanation:

* The program starts by declaring three matrices M1, M2, and M3, which are all of size 5x5.

* It then reads the values of the matrices M1 and M2 from the user.

* It initializes the matrix M3 with zeros.

* It calculates the transpose of matrix M1 and stores it in matrix M3.

* It calculates the sum of matrices M3 and M2 and stores the result in matrix M3.

* Finally, it prints the matrix M3 to the console.