```portugal
PROGRAM MediaAlunos;

VAR
  media, acumulador, contador: REAL;
  nota: REAL;

BEGIN
  contador := 0;
  acumulador := 0;

  WHILE contador < 3 DO
  BEGIN
    Writeln('Informe a nota do aluno ', contador + 1);
    Readln(nota);

    acumulador := acumulador + nota;
    contador := contador + 1;
  END;

  media := acumulador / contador;

  Writeln('A média dos alunos é ', media);
END.
```

This code calculates the average of the grades of three students.

The program first declares four variables:

* `media`: This variable will store the average of the grades.
* `acumulador`: This variable will store the sum of the grades.
* `contador`: This variable will store the number of students.
* `nota`: This variable will store the grade of a single student.

The program then initializes the value of `contador` and `acumulador` to 0.

The program then enters a loop that will run three times. In each iteration of the loop, the program will:

* Prompt the user to enter the grade of a student.
* Read the grade of the student.
* Add the grade of the student to the `acumulador` variable.
* Increment the `contador` variable.

After the loop has finished, the program will calculate the average of the grades by dividing the value of the `acumulador` variable by the value of the `contador` variable.

The program will then display the average of the grades on the screen.