```
PROGRAM CALCULAR_MEDIA

DECLARE
  nota1, nota2, nota3: REAL;
  media: REAL;

BEGIN
  WRITE("Digite a primeira nota: ");
  READ(nota1);
  WRITE("Digite a segunda nota: ");
  READ(nota2);
  WRITE("Digite a terceira nota: ");
  READ(nota3);

  media := (nota1 + nota2 + nota3) / 3;

  IF media >= 7 THEN
    WRITE("Aluno aprovado com média ", media);
  ELSE
    WRITE("Aluno reprovado com média ", media);
  END IF;

END PROGRAM;
```

Explanation:

1. **Program Header**: The program starts with the `PROGRAM` statement, which specifies the name of the program (`CALCULAR_MEDIA`).

2. **Variable Declarations**: The `DECLARE` statement is used to declare the variables used in the program. In this case, we have four real variables: `nota1`, `nota2`, `nota3`, and `media`.

3. **Input**: The `WRITE` and `READ` statements are used to interact with the user and obtain the values for the three grades.

4. **Calculating the Average**: The `media` variable is calculated by taking the average of the three grades using the formula: `(nota1 + nota2 + nota3) / 3`.

5. **Conditional Statement**: The `IF` statement is used to check if the average is greater than or equal to 7. If it is, the student is considered approved, otherwise they are considered reproved.

6. **Output**: The `WRITE` statement is used to display the calculated average and whether the student is approved or reproved.