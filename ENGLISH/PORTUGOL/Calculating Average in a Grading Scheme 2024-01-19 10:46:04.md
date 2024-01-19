```portugol
PROGRAM CALCULANDO_MEDIA_EM_PROGRAMA_DE_PONTOS;

INICIO
    LER N;
    ESCREVA "Os nomes dos alunos sao:";
    ESCREVA;
    PARA I DE 1 ATE N FACA
        LER NOME;
        ESCREVA NOME
    FIM-PARA;
    ESCREVA;
    PARA I DE 1 ATE N FACA
        LER NOTA;
        ESCREVA NOTA
    FIM-PARA;
    ESCREVA;
    PARA I DE 1 ATE N FACA
        CALCULAR MEDIA;
        ESCREVA MEDIA
    FIM-PARA;
    ESCREVA;
    CALCULAR MEDIA_TOTAL;
    ESCREVA MEDIA_TOTAL;
FIM-PROGRAMA.
```

This program calculates the average of a list of students' grades.

- The first line of the program reads the number of students.
- The next line prints the message "Os nomes dos alunos sao:".
- The following loop reads the names of the students and prints them.
- After the loop, the message "Os notas dos alunos sao:" is printed.
- The next loop reads the grades of the students and prints them.
- After the loop, the message "As medias dos alunos sao:" is printed.
- The following loop calculates the average of the students' grades and prints it.
- After the loop, the message "A media total dos alunos e:" is printed.
- The final line of the program calculates the total average of the students' grades and prints it.

Here is a more detailed explanation of the code:

- The `LER` statement reads input from the user.
- The `ESCREVA` statement prints output to the user.
- The `PARA` statement creates a loop that iterates a specified number of times.
- The `CALCULAR` statement calculates a value.
- The `FIM-PARA` statement ends a loop.
- The `FIM-PROGRAMA` statement ends the program.

The `MEDIA` variable is calculated as the sum of the student's grades divided by the number of grades. The `MEDIA_TOTAL` variable is calculated as the sum of all the students' averages divided by the number of students.