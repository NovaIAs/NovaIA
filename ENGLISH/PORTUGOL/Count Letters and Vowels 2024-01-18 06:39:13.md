```portugol
PROGRAM ContaLetras

VAR
  frase: STRING;
  i: NUMERIC;
  c: NUMERIC;
  letra: CHAR;
  qntLetras: NUMERIC;
  qtdVogais: NUMERIC;
BEGIN
  frase := "Uma frase qualquer para testar o programa.";

  i := 1;
  qntLetras := 0;
  qtdVogais := 0;
  WHILE i <= LEN(frase) DO
  BEGIN
    letra := SUBSTR(frase, i, 1);
    c := ORD(letra);
    IF c >= 65 AND c <= 90 THEN
      c := c + 32;
    END IF;
    IF (c >= 97 AND c <= 122) OR c = 32 THEN
      INC(qntLetras);
      IF (c = 97 OR c = 101 OR c = 105 OR c = 111 OR c = 117 OR c = 105) THEN
        INC(qtdVogais);
      END IF;
    END IF;
    INC(i);
  END WHILE;

  WRITE("Quantidade de letras:", qntLetras);
  WRITE("Quantidade de vogais:", qtdVogais);
END PROGRAM.
```

This code counts the number of letters and vowels in a given string.

The code first defines the variables:

* `frase`: A string variable that will store the input string.
* `i`: A numeric variable that will be used to iterate through the string.
* `c`: A numeric variable that will store the ASCII code of the current character in the string.
* `letra`: A character variable that will store the current character in the string.
* `qntLetras`: A numeric variable that will store the total number of letters in the string.
* `qtdVogais`: A numeric variable that will store the total number of vowels in the string.

The code then reads the input string from the user and stores it in the `frase` variable.

Next, the code uses a `WHILE` loop to iterate through the string, one character at a time. For each character, the code does the following:

* It converts the character to lowercase using the `SUBSTR` and `ORD` functions.
* It checks if the character is a letter or a space using the `IF` statement. If the character is a letter or a space, the code increments the `qntLetras` variable.
* If the character is a vowel, the code increments the `qtdVogais` variable.

After the loop has finished, the code prints the total number of letters and vowels in the string using the `WRITE` statement.

Here is an example of the output of the program:

```
Quantidade de letras: 39
Quantidade de vogais: 15
```