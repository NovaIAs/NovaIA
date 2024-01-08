```portugol
program CALCULAR_NOTA_FINAL;

var
  nota1, nota2, nota3: real;
  media: real;

begin
  writeln('Digite a primeira nota: ');
  readln(nota1);
  writeln('Digite a segunda nota: ');
  readln(nota2);
  writeln('Digite a terceira nota: ');
  readln(nota3);

  media := (nota1 + nota2 + nota3) / 3;

  if media >= 7 then
    writeln('Aluno aprovado com média ', media:6:2);
  elsif media >= 5 and media < 7 then
    writeln('Aluno em recuperação com média ', media:6:2);
  else
    writeln('Aluno reprovado com média ', media:6:2);
end.
```

This program calculates the final grade of a student based on three grades.

The program starts by declaring the variables that will be used:

* nota1, nota2, nota3: the three grades of the student
* media: the final grade of the student

The program then asks the user to enter the three grades.

The program then calculates the final grade by taking the average of the three grades.

The program then uses an if-else statement to determine the status of the student. If the final grade is greater than or equal to 7, the student is approved. If the final grade is between 5 and 7, the student is in recovery. If the final grade is less than 5, the student is failed.

The program then displays the final grade of the student and their status.