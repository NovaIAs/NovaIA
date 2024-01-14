```portugol
PROGRAMA EXEMPLO;
VARIAVEIS
    NUMEROINTEIRO: INTEGER;
    NUMEROREAL: REAL;
    LOGICO: BOOLEAN;
    CARACTERE: CHAR;
    PALAVRA: STRING;
    VETOR: ARRAY[1..10] OF INTEGER;
    MATRIZ: ARRAY[1..10, 1..10] OF REAL;
INICIO
    NUMEROINTEIRO := 10;
    NUMEROREAL := 10.5;
    LOGICO := TRUE;
    CARACTERE := 'A';
    PALAVRA := 'EXEMPLO';
    FOR I := 1 TO 10 DO
        VETOR[I] := I;
    END FOR;
    FOR I := 1 TO 10 DO
        FOR J := 1 TO 10 DO
            MATRIZ[I, J] := I + J;
        END FOR;
    END FOR;
    WRITE('NUMERO INTEIRO: ', NUMEROINTEIRO);
    WRITE('NUMERO REAL: ', NUMEROREAL);
    WRITE('LOGICO: ', LOGICO);
    WRITE('CARACTERE: ', CARACTERE);
    WRITE('PALAVRA: ', PALAVRA);
    WRITE('VETOR: ');
    FOR I := 1 TO 10 DO
        WRITE(VETOR[I], ' ');
    END FOR;
    WRITE('MATRIZ: ');
    FOR I := 1 TO 10 DO
        FOR J := 1 TO 10 DO
            WRITE(MATRIZ[I, J], ' ');
        END FOR;
        WRITE();
    END FOR;
FIM.
```

Here's an explanation of the code:

1. **Program Structure**:
   - The program starts with the `PROGRAMA EXEMPLO;` statement, which declares the name of the program as "EXEMPLO".
   - The `VARIAVEIS` section declares variables of different types, including integers, real numbers, Boolean, characters, strings, arrays, and matrices.
   - The `INICIO` and `FIM.` statements mark the beginning and end of the program, respectively.

2. **Variable Declarations**:
   - `NUMEROINTEIRO` is an integer variable initialized to 10.
   - `NUMEROREAL` is a real (floating-point) variable initialized to 10.5.
   - `LOGICO` is a Boolean variable initialized to TRUE.
   - `CARACTERE` is a character variable initialized to 'A'.
   - `PALAVRA` is a string variable initialized to 'EXEMPLO'.
   - `VETOR` is an array of integers with 10 elements, and each element is initialized to its index value.
   - `MATRIZ` is a two-dimensional array of real numbers with 10 rows and 10 columns, and each element is initialized to the sum of its row and column indices.

3. **Output Statements**:
   - The `WRITE` statements are used to display the values of the variables and arrays on the console.
   - The `WRITE` statement with a comma-separated list of arguments prints the values of the arguments, separated by a space.
   - The `WRITE()` statement without any arguments prints a newline.

4. **Loops**:
   - The `FOR` loops are used to iterate through the elements of the array and matrix.
   - The `FOR I := 1 TO 10 DO` loop iterates from 1 to 10, and the variable `I` is used as the loop counter.
   - The nested `FOR` loop iterates through the rows and columns of the matrix.

5. **Comments**:
   - Comments are used to provide additional information about the code.
   - Comments start with `//` and continue until the end of the line.

This code demonstrates the use of various data types, arrays, matrices, loops, and output statements in Portugol. It's a more complex and comprehensive example compared to the previous one.