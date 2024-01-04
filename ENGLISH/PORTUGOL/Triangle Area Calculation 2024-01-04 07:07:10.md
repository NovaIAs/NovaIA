```portugol
PROGRAMA CalculaAreaTriangulo;

VAR
  Base, Altura, Area: REAL;

INICIO
  ESCREVA("Digite a base do triangulo: ");
  LEIA(Base);
  ESCREVA("Digite a altura do triangulo: ");
  LEIA(Altura);

  Area := (Base * Altura) / 2;

  ESCREVA("A area do triangulo e: ", Area);

FIM.
```

Explanation:

* The program starts with the line `PROGRAMA CalculaAreaTriangulo;`, which declares the name of the program.
* The next line declares the variables that will be used in the program:
    * `Base`, `Altura`, and `Area` are all of type `REAL`, which is a floating-point data type.
* The next line uses the `ESCREVA` statement to print a message to the console asking the user to enter the base of the triangle.
* The next line uses the `LEIA` statement to read the user's input and store it in the variable `Base`.
* The next line uses the `ESCREVA` statement to print a message to the console asking the user to enter the height of the triangle.
* The next line uses the `LEIA` statement to read the user's input and store it in the variable `Altura`.
* The next line calculates the area of the triangle using the formula `Area := (Base * Altura) / 2;`.
* The next line uses the `ESCREVA` statement to print the area of the triangle to the console.
* The last line in the program is the `FIM.` statement, which indicates the end of the program.