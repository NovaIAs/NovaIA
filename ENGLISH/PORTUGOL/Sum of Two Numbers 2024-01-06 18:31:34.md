```portugal
PROGRAM SomaNumeros;

VAR
  Numero1, Numero2, Soma : INTEGER;

BEGIN
  Write('Digite o primeiro número: ');
  Readln(Numero1);
  Write('Digite o segundo número: ');
  Readln(Numero2);

  Soma := Numero1 + Numero2;

  Writeln('A soma dos números é: ', Soma);
END.
```

This code is a simple program that asks the user to enter two numbers, adds them together, and then displays the result. The program uses the following variables:

* Numero1: The first number entered by the user.
* Numero2: The second number entered by the user.
* Soma: The sum of the two numbers.

The program first uses the Write() function to display a message to the user asking them to enter the first number. The Readln() function is then used to read the user's input and store it in the Numero1 variable.

The program then uses the Write() function to display a message to the user asking them to enter the second number. The Readln() function is then used to read the user's input and store it in the Numero2 variable.

The program then uses the := operator to assign the sum of the two numbers to the Soma variable.

Finally, the program uses the Writeln() function to display the result of the addition to the user.