```portugol
PROGRAM MAIOR_VALOR;

VAR
   A, B, C, MAIOR : REAL;

BEGIN
   WRITE('DIGITE O PRIMEIRO VALOR: ');
   READ(A);
   WRITE('DIGITE O SEGUNDO VALOR: ');
   READ(B);
   WRITE('DIGITE O TERCEIRO VALOR: ');
   READ(C);

   IF (A > B) THEN
      MAIOR := A;
   ELSE
      MAIOR := B;
   END;

   IF (MAIOR < C) THEN
      MAIOR := C;
   END;

   WRITE('O MAIOR VALOR É: ', MAIOR);
END.
```

This program asks the user to input three real numbers (A, B, and C) and then finds and displays the largest of the three numbers.

The program first uses the IF statement to compare A and B. If A is greater than B, then MAIOR is assigned the value of A. Otherwise, MAIOR is assigned the value of B.

Next, the program uses another IF statement to compare MAIOR and C. If MAIOR is less than C, then MAIOR is assigned the value of C.

Finally, the program uses the WRITE statement to display the value of MAIOR, which is the largest of the three numbers entered by the user.