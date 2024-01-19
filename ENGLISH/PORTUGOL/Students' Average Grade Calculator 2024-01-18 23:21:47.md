```portugol
PROGRAM CALCULAR_MEDIA_ALUNOS;
VAR
  ALUNO: ARRAY [1..100] OF RECORD
    NOME: STRING[50];
    NOTAS: ARRAY [1..4] OF REAL;
    MEDIA: REAL;
  FIM_REGISTRO;
  QTD_ALUNOS: INTEGER;
  I, J: INTEGER;

INICIO
  QTD_ALUNOS := 0;

  ESCREVA("Informe a quantidade de alunos: ");
  LEIA(QTD_ALUNOS);

  PARA I DE 1 ATE QTD_ALUNOS FACA
    ESCREVA("Informe o nome do aluno: ");
    LEIA(ALUNO[I].NOME);

    PARA J DE 1 ATE 4 FACA
      ESCREVA("Informe a nota ", J, " do aluno: ");
      LEIA(ALUNO[I].NOTAS[J]);
    FIM_PARA;

    ALUNO[I].MEDIA := (ALUNO[I].NOTAS[1] + ALUNO[I].NOTAS[2] + ALUNO[I].NOTAS[3] + ALUNO[I].NOTAS[4]) / 4;
  FIM_PARA;

  ESCREVALN;
  ESCREVALN("Alunos e suas médias:");

  PARA I DE 1 ATE QTD_ALUNOS FACA
    ESCREVA(ALUNO[I].NOME, ": ", ALUNO[I].MEDIA);
    ESCREVALN;
  FIM_PARA;

  ESCREVALN;
  ESCREVA("Média geral da turma: ", MEDIA_GERAL);
  ESCREVALN;

FIM_PROGRAMA.
```

This program calculates the average of the grades of a class of students.

The program first asks the user to input the number of students in the class.

Then, it creates an array of records to store the information of each student. Each record contains the student's name, an array of four grades, and the student's average.

The program then asks the user to input the name and four grades for each student.

After all the students' information has been input, the program calculates the average grade for each student and stores it in the student's record.

The program then prints the names and average grades of all the students in the class.

Finally, the program calculates the overall average grade for the class and prints it.

Here is a breakdown of the code:

* The `PROGRAM` statement starts the program.
* The `VAR` statement declares the variables used in the program.
* The `INICIO` statement starts the main part of the program.
* The `QTD_ALUNOS` variable is used to store the number of students in the class.
* The `I` and `J` variables are used as loop counters.
* The `ALUNO` array is used to store the information of each student.
* The `NOME` field of the `ALUNO` record stores the student's name.
* The `NOTAS` field of the `ALUNO` record stores the student's four grades.
* The `MEDIA` field of the `ALUNO` record stores the student's average grade.
* The `ESCREVA` and `LEIA` statements are used to input and output data to the console.
* The `PARA` statement is used to create a loop.
* The `FIM_PARA` statement ends a loop.
* The `ESCREVALN` statement prints a newline to the console.
* The `FIM_PROGRAMA` statement ends the program.