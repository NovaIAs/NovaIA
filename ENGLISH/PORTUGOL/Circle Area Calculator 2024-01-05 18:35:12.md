```portugol
PROGRAMA CalcularAreaCirculo;

VAR
  RaioCirculo: REAL;
  AreaCirculo: REAL;
  Pi: REAL;

INICIO
  Pi := 3.14159;

  ESCREVA("Digite o raio do círculo: ");
  LEIA(RaioCirculo);

  AreaCirculo := Pi * RaioCirculo * RaioCirculo;

  ESCREVA("A área do círculo é: ", AreaCirculo);
FIM.
```

This program calculates the area of a circle.

The program first declares the variables that it will use:

* `RaioCirculo`: This variable will store the radius of the circle.
* `AreaCirculo`: This variable will store the area of the circle.
* `Pi`: This variable will store the value of pi.

The program then prompts the user to enter the radius of the circle.

```portugol
ESCREVA("Digite o raio do círculo: ");
LEIA(RaioCirculo);
```

The program then calculates the area of the circle using the formula `Area = π * Raio²`.

```portugol
AreaCirculo := Pi * RaioCirculo * RaioCirculo;
```

Finally, the program displays the area of the circle to the user.

```portugol
ESCREVA("A área do círculo é: ", AreaCirculo);
```