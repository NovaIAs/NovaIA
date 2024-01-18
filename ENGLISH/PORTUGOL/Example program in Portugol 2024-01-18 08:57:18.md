```portugol
PROGRAMA exemplo;
{ Declaração de variáveis }
VAR
    inteiro: x, y, z;
    real: m, n;
    logico: b;
    texto: str;
    vetor: vec[1..10] de inteiro;
    matriz: mat[1..10, 1..10] de inteiro;

{ Início do programa }
INICIO
    { Atribuição de valores às variáveis }
    x := 10;
    y := 20;
    z := x + y;
    m := 3.14159265;
    n := 2.7182818284;
    b := VERDADEIRO;
    str := "Exemplo";
    vec[1] := 1;
    vec[2] := 2;
    vec[3] := 3;
    mat[1, 1] := 1;
    mat[1, 2] := 2;
    mat[1, 3] := 3;
    mat[2, 1] := 4;
    mat[2, 2] := 5;
    mat[2, 3] := 6;

    { Operações matemáticas }
    x := x + y;
    y := x - z;
    z := x * y;
    m := m / n;

    { Operações lógicas }
    b := NAO b;
    b := b E VERDADEIRO;
    b := b OU FALSO;

    { Operações de entrada e saída }
    ESCREVA("O valor de x é: ", x);
    LEIA(str);
    ESCREVA("A string digitada foi: ", str);

    { Estruturas de controle }
    SE b ENTAO
        ESCREVA("O valor de b é verdadeiro");
    SENÃO
        ESCREVA("O valor de b é falso");
    FIM_SE;

    PARA c DE 1 ATE 10 FACA
        ESCREVA("O valor de c é: ", c);
    FIM_PARA;

    ENQUANTO b FACA
        ESCREVA("O valor de b é verdadeiro");
        b := NAO b;
    FIM_ENQUANTO;

{ Fim do programa }
FIM.
```

Explicação do código:

* O programa começa com a declaração de variáveis. São declaradas variáveis de diferentes tipos, como inteiro, real, lógico, texto, vetor e matriz.
* Em seguida, são atribuídos valores às variáveis.
* São realizadas operações matemáticas, como adição, subtração, multiplicação e divisão.
* Também são realizadas operações lógicas, como negação, conjunção e disjunção.
* São realizadas operações de entrada e saída, como leitura e escrita de dados.
* São utilizadas estruturas de controle, como o comando SE, o comando PARA e o comando ENQUANTO.
* O programa termina com o comando FIM.