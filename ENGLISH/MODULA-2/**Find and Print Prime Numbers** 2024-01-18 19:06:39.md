```modula-2

MODULE PrintPrimes;
  FROM FormatString IMPORT Fmt;
  FROM Terminal IMPORT WriteString, WriteLn, WriteInt;

  PROC PrintPrimes(UPPER : CARDINAL);
  VAR N, PRIME, LASTPRIME, REMAINDER : CARDINAL;
  BEGIN
    WriteString("Prime numbers from 2 to ");
    WriteInt(UPPER, 0);
    WriteLn;
    N := 2;
    LASTPRIME := 0;
    WHILE N < UPPER DO
      PRIME := TRUE;
      REMAINDER := LASTPRIME + 1;
      WHILE PRIME AND REMAINDER <= SQRT(N) DO
        IF N MOD REMAINDER = 0 THEN
          PRIME := FALSE;
        ELSE
          INC(REMAINDER);
        FI;
      OD;

      IF PRIME THEN
        Fmt(1, "(10i0,F1)", N, "");
        INC(LASTPRIME);
      FI;
      INC(N);
    OD;
  END PrintPrimes;

  PROCEDURE CalculatePrimes;
    CONST UPPER : CARDINAL := 100;
  BEGIN
    PrintPrimes(UPPER);
  END CalculatePrimes;

BEGIN
  CalculatePrimes;
END PrintPrimes.

```

This code finds and prints all the prime numbers from 2 to a given upper limit. It starts by finding the first prime number, which is 2. Then, it uses a loop to test all the numbers from 3 to the upper limit. For each number, it divides it by all the prime numbers found so far. If any of these divisions result in a remainder of 0, then the number is not prime. Otherwise, it is prime and is added to the list of prime numbers. The code then prints the list of prime numbers.

Here is an explanation of the code:

* The `PrintPrimes` procedure takes one parameter, `UPPER`, which specifies the upper limit of the prime numbers to be found.
* The `PrintPrimes` procedure first uses the `WriteString` and `WriteInt` procedures to print a message to the console indicating that it is printing the prime numbers from 2 to the upper limit.
* The `PrintPrimes` procedure then initializes several variables:
    * `N` is the number that is being tested for primality.
    * `PRIME` is a Boolean variable that indicates whether or not `N` is prime.
    * `LASTPRIME` is the largest prime number that has been found so far.
    * `REMAINDER` is the remainder of the division of `N` by the current prime number being tested.
* The `PrintPrimes` procedure then enters a `WHILE` loop that continues until `N` is greater than or equal to the upper limit.
* Inside the `WHILE` loop, the `PrintPrimes` procedure first sets the `PRIME` variable to `TRUE`, indicating that `N` is prime.
* The `PrintPrimes` procedure then enters a nested `WHILE` loop that continues until `REMAINDER` is greater than the square root of `N`.
* Inside the nested `WHILE` loop, the `PrintPrimes` procedure divides `N` by `REMAINDER` and checks the remainder. If the remainder is 0, then `N` is not prime, and the `PRIME` variable is set to `FALSE`.
* If the `PRIME` variable is still `TRUE` after the nested `WHILE` loop, then `N` is prime, and the `PrintPrimes` procedure prints `N` using the `Fmt` procedure.
* The `PrintPrimes` procedure then increments `N` and repeats the `WHILE` loop.
* The `CalculatePrimes` procedure calls the `PrintPrimes` procedure with an upper limit of 100.
* The `PrintPrimes` procedure prints the list of prime numbers from 2 to 100.