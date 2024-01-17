```portugal
PROGRAMA verificadorDeNumerosPrimos;

FUNCAO isDivisibleBy(radicand, divisor : Inteiro) : Logico;
VAR
  i: Inteiro;
BEGIN
  isDivisibleBy := FALSO;
  FOR i := 2 TO radicand - 1 DO
    IF radicand MOD i = 0 THEN
      isDivisibleBy := VERDADEIRO;
      EXIT;
    ENDIF;
  ENDFOR;
END;

FUNCAO isPrimeNumber(number : Inteiro) : Logico;
VAR
  radicand, divisor: Inteiro;
BEGIN
  isPrimeNumber := VERDADEIRO;
  radicand := number;
  IF radicand < 2 THEN
    isPrimeNumber := FALSO;
  ELSE
    divisor := 2;
    WHILE divisor <= radicand DIV 2 AND isPrimeNumber DO
      isPrimeNumber := NOT isDivisibleBy(radicand, divisor);
      divisor := divisor + 1;
    ENDWHILE;
  ENDIF;
END;

VAR
  number: Inteiro;
BEGIN
  ESCREVA("Digite um número: ");
  LEIA(number);
  SE isPrimeNumber(number) ENTAO
    ESCREVA(number, " é um número primo.");
  SENAO
    ESCREVA(number, " não é um número primo.");
  FIMSE;
END.
```

This program checks if a given number is prime.

The program defines two functions:

* `isDivisibleBy()` checks if a number is divisible by another number.
* `isPrimeNumber()` checks if a number is prime.

The main program asks the user to input a number, then calls the `isPrimeNumber()` function to check if the number is prime. If the number is prime, the program prints a message saying so. If the number is not prime, the program prints a message saying so.

Here is a step-by-step explanation of the code:

* The `isDivisibleBy()` function takes two integers as input: the radicand (the number being checked for divisibility) and the divisor. The function returns a Boolean value indicating whether the radicand is divisible by the divisor.
* The function initializes a local variable `i` to 2.
* The function uses a `FOR` loop to iterate from 2 to the radicand - 1.
* Inside the loop, the function checks if the radicand modulo the divisor is equal to 0. If it is, then the function sets the `isDivisibleBy` variable to TRUE and exits the loop using the `EXIT` statement.
* If the loop completes without finding a divisor, then the function sets the `isDivisibleBy` variable to FALSE.
* The `isPrimeNumber()` function takes an integer as input and returns a Boolean value indicating whether the number is prime.
* The function initializes two local variables: `radicand` and `divisor`. The `radicand` variable is set to the input number, and the `divisor` variable is set to 2.
* The function checks if the radicand is less than 2. If it is, then the function sets the `isPrimeNumber` variable to FALSE and returns.
* The function enters a `WHILE` loop that continues as long as the divisor is less than or equal to half the radicand and the `isPrimeNumber` variable is TRUE.
* Inside the loop, the function calls the `isDivisibleBy()` function to check if the radicand is divisible by the divisor. If it is, then the function sets the `isPrimeNumber` variable to FALSE and exits the loop using the `EXIT` statement.
* If the loop completes without finding a divisor, then the function sets the `isPrimeNumber` variable to TRUE.
* The main program asks the user to input a number using the `ESCREVA()` and `LEIA()` functions.
* The main program calls the `isPrimeNumber()` function to check if the number is prime.
* If the number is prime, then the main program prints a message saying so using the `ESCREVA()` function.
* If the number is not prime, then the main program prints a message saying so using the `ESCREVA()` function.