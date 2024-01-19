```portugol
programa Exemplo;
função Principal(): inteiro;
variáveis
   inteiro A = 10;
   inteiro B = 20;
   inteiro C = 30;
início
   escreva("O valor de A é: ", A);
   escreva("O valor de B é: ", B);
   escreva("O valor de C é: ", C);
   A := A + 1;
   B := B + 2;
   C := C + 3;
   escreva("O novo valor de A é: ", A);
   escreva("O novo valor de B é: ", B);
   escreva("O novo valor de C é: ", C);
   se (A > B) então
      escreva("A é maior que B");
   senão
      escreva("B é maior ou igual a A");
   fim-se;
   se (A < C) então
      escreva("A é menor que C");
   senão
      escreva("C é menor ou igual a A");
   fim-se;
   retorne 0;
fim função;
fim programa.
```

Explanation:

- The program starts with the `programa` keyword, followed by the program name `Exemplo`.

- The `função Principal()` line defines the main function of the program. This is where the execution of the program begins.

- The `variáveis` section declares the variables used in the program. In this case, we have three integer variables: `A`, `B`, and `C`.

- The `início` line marks the beginning of the executable code.

- The `escreva()` function is used to print output to the console. In this case, we are using it to print the values of the variables `A`, `B`, and `C`.

- The `A := A + 1` line increments the value of `A` by 1.

- The `B := B + 2` line increments the value of `B` by 2.

- The `C := C + 3` line increments the value of `C` by 3.

- The `se (A > B) então` line starts a conditional statement. This statement checks if the value of `A` is greater than the value of `B`.

- If the condition is true, the `escreva()` function is used to print the message "A é maior que B".

- If the condition is false, the `senão` block is executed. In this block, the `escreva()` function is used to print the message "B é maior ou igual a A".

- The `se (A < C) então` line starts another conditional statement. This statement checks if the value of `A` is less than the value of `C`.

- If the condition is true, the `escreva()` function is used to print the message "A é menor que C".

- If the condition is false, the `senão` block is executed. In this block, the `escreva()` function is used to print the message "C é menor ou igual a A".

- The `retorne 0` line returns 0 from the main function, indicating that the program has executed successfully.

- The `fim função` line marks the end of the main function.

- The `fim programa` line marks the end of the program.