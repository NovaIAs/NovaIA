```portugol
program processar_dados;
var
    arq_entrada: file;
    arq_saida: file;
    linha: string;
    nome, sobrenome: string;
    idade: integer;
    nota1, nota2: real;
    media: real;
    situacao: string;
begin
    assign(arq_entrada, "dados.txt");
    reset(arq_entrada);
    assign(arq_saida, "saida.txt");
    rewrite(arq_saida);
    while not eof(arq_entrada) do
    begin
        readln(arq_entrada, linha);
        sscanf(linha, "%s %s %d %f %f", nome, sobrenome, idade, nota1, nota2);
        media := (nota1 + nota2) / 2;
        if media >= 7 then
            situacao := "Aprovado"
        else
            situacao := "Reprovado";
        writeln(arq_saida, nome, sobrenome, idade, media, situacao);
    end;
    close(arq_entrada);
    close(arq_saida);
end.
```

**Explanation:**

This program reads data from a text file called "dados.txt" and processes it. The data in the text file is expected to be in the following format:

```
"John Doe 20 8.5 9.0"
"Jane Smith 22 7.8 8.2"
"Michael Jones 24 9.1 8.7"
```

Each line in the text file contains the following information:

* The first and last name of a student
* The student's age
* The student's two grades

The program reads each line of the text file and parses the data into individual variables. It then calculates the student's average grade and determines whether the student is approved or reproved. The results are then written to a new text file called "saida.txt".

Here is a detailed explanation of the code:

* The `var` statement declares the variables used in the program.
* The `assign` statement assigns the name of the input and output files to the `arq_entrada` and `arq_saida` variables, respectively.
* The `reset` statement opens the input file for reading.
* The `rewrite` statement opens the output file for writing.
* The `while not eof(arq_entrada)` loop reads each line of the input file until the end of the file is reached.
* The `readln(arq_entrada, linha);` statement reads a line of text from the input file and stores it in the `linha` variable.
* The `sscanf(linha, "%s %s %d %f %f", nome, sobrenome, idade, nota1, nota2);` statement parses the line of text and extracts the student's name, age, and two grades.
* The `media := (nota1 + nota2) / 2;` statement calculates the student's average grade.
* The `if media >= 7 then` statement checks if the student's average grade is greater than or equal to 7.
* If the student's average grade is greater than or equal to 7, the `situacao := "Aprovado"` statement sets the student's status to "Aprovado".
* Otherwise, the `situacao := "Reprovado"` statement sets the student's status to "Reprovado".
* The `writeln(arq_saida, nome, sobrenome, idade, media, situacao);` statement writes the student's name, age, average grade, and status to the output file.
* The `close(arq_entrada);` statement closes the input file.
* The `close(arq_saida);` statement closes the output file.