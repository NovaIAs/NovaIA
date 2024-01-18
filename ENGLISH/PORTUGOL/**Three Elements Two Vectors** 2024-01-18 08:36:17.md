```portugol
 PROGRAMA TresElementosDoisVetores /* Programa que solicita que o usuário entre com dois vetores contendo 3 elementos e exibe 6 mensagens com os elementos combinados de acordo com o enunciado do problema*/
 VAR
     V1, V2: VETOR[1..3] DE INTEIRO;
     i: INTEIRO;
 INICIO
     PARA i <- 1 ATE 3 FACA
         ESCREVA("Entre com o elemento ", i, "do primeiro vetor:");
         LEIA(V1[i]);
     FIMPARA;
     PARA i <- 1 ATE 3 FACA
         ESCREVA("Entre com o elemento ", i, "do segundo vetor:");
         LEIA(V2[i]);
     FIMPARA;
     ESCREVA(V1[1], " + ", V2[1], " = ", V1[1] + V2[1]);
     ESCREVA(V1[1], " + ", V2[2], " = ", V1[1] + V2[2]);
     ESCREVA(V1[1], " + ", V2[3], " = ", V1[1] + V2[3]);
     ESCREVA(V1[2], " + ", V2[1], " = ", V1[2] + V2[1]);
     ESCREVA(V1[2], " + ", V2[2], " = ", V1[2] + V2[2]);
     ESCREVA(V1[2], " + ", V2[3], " = ", V1[2] + V2[3]);
     ESCREVA(V1[3], " + ", V2[1], " = ", V1[3] + V2[1]);
     ESCREVA(V1[3], " + ", V2[2], " = ", V1[3] + V2[2]);
     ESCREVA(V1[3], " + ", V2[3], " = ", V1[3] + V2[3]);
 FIMPROGRAMA

```

The code you requested is a program in PORTUGOL that asks the user to input two vectors containing three elements each and displays six messages with the elements combined according to the statement of the problem.

The code starts by declaring two vectors, `V1` and `V2`, of three integer elements each. It then uses a `FOR` loop to read the three elements of each vector from the user.

Next, the code uses another `FOR` loop to display six messages, each showing the sum of two elements from the two vectors. The messages are displayed in the following order:

* `V1[1] + V2[1] = V1[1] + V2[1]`
* `V1[1] + V2[2] = V1[1] + V2[2]`
* `V1[1] + V2[3] = V1[1] + V2[3]`
* `V1[2] + V2[1] = V1[2] + V2[1]`
* `V1[2] + V2[2] = V1[2] + V2[2]`
* `V1[2] + V2[3] = V1[2] + V2[3]`
* `V1[3] + V2[1] = V1[3] + V2[1]`
* `V1[3] + V2[2] = V1[3] + V2[2]`
* `V1[3] + V2[3] = V1[3] + V2[3]`

Finally, the program ends.