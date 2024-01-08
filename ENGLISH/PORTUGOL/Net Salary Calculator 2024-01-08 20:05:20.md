```portugol
PROGRAMA "Calcula o salário líquido de um funcionário"

INÍCIO
    DECLARE
        salario_bruto REAL;
        descontos REAL;
        salario_liquido REAL;

    ESCREVA("Informe o salário bruto: ");
    LEIA(salario_bruto);

    ESCREVA("Informe os descontos: ");
    LEIA(descontos);

    salario_liquido := salario_bruto - descontos;

    ESCREVA("O salário líquido é: ", salario_liquido);

FIM
```

This program calculates the net salary of an employee.

The program first declares the variables that will be used in the program:

* `salario_bruto`: The gross salary of the employee.
* `descontos`: The discounts that are applied to the gross salary.
* `salario_liquido`: The net salary of the employee.

The program then prompts the user to enter the gross salary and the discounts.

The program then calculates the net salary by subtracting the discounts from the gross salary.

Finally, the program displays the net salary to the user.