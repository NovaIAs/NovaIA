```portugol
PROGRAMA CALCULAR_MEDIA;
VAR
    NOTA1, NOTA2, MEDIA: REAL;
INICIO
    ESCREVER("Digite a primeira nota: ");
    LER(NOTA1);
    ESCREVER("Digite a segunda nota: ");
    LER(NOTA2);
    MEDIA := (NOTA1 + NOTA2) / 2;
    SE MEDIA >= 7 ENTAO
        ESCREVER("Aluno aprovado com média ", MEDIA);
    SENAO
        ESCREVER("Aluno reprovado com média ", MEDIA);
    FIMSE;
FIMPROGRAMA.
```

This program calculates the average of two grades and prints whether the student was approved or not.

The program starts by declaring the variables `NOTA1`, `NOTA2`, and `MEDIA`, which will store the grades and the average, respectively.

Then, it uses the `ESCREVER` statement to print a message asking the user to enter the first grade. The `LER` statement is used to read the input from the user and store it in the variable `NOTA1`.

The same process is repeated for the second grade.

After that, the program calculates the average of the two grades by adding them together and dividing by 2. The result is stored in the variable `MEDIA`.

The program then uses the `SE` statement to check if the average is greater than or equal to 7. If it is, the program prints a message saying that the student was approved, along with the average.

If the average is less than 7, the program prints a message saying that the student was reproved, along with the average.

Finally, the program uses the `FIMPROGRAMA` statement to end the program.