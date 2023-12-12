Claro! Aqui está um código complexo em Portugol:

```
algoritmo codigoComplexo;

funcao primo(num: inteiro): logico;
var
   i: inteiro;

inicio
   se num < 2 entao
      retorne falso;
   fimse

   para i de 2 ate num-1 faca
      se num % i = 0 entao
         retorne falso;
      fimse
   fimpara

   retorne verdadeiro;
fimfuncao

funcao somaPrimos(n: inteiro): inteiro;
var
   i, contador, soma: inteiro;

inicio
   i <- 2;
   contador <- 0;
   soma <- 0;

   enquanto contador < n faca
      se primo(i) entao
         soma <- soma + i;
         contador <- contador + 1;
      fimse

      i <- i + 1;
   fimenquanto

   retorne soma;
fimfuncao

funcao fatorial(num: inteiro): inteiro;
var
   resultado, i: inteiro;

inicio
   resultado <- 1;

   para i de 1 ate num faca
      resultado <- resultado * i;
   fimpara

   retorne resultado;
fimfuncao

procedimento exibirTabuada(num: inteiro);
var
   i: inteiro;

inicio
   para i de 1 ate 10 faca
      escreva(num, " x ", i, " = ", num * i);
      escreval("");
   fimpara
fimprocedimento

procedimento main;
var
   opcao: inteiro;
   numero, quantidade: inteiro;

inicio
   repita
      escreval("Selecione uma opcao:");
      escreval("1 - Verificar se um numero eh primo");
      escreval("2 - Calcular a soma dos primeiros n numeros primos");
      escreval("3 - Calcular o fatorial de um numero");
      escreval("4 - Exibir a tabuada de um numero");
      escreval("5 - Sair");

      leia(opcao);

      escolha opcao
         caso 1:
            escreval("");
            escreva("Digite um numero: ");
            leia(numero);

            se primo(numero) entao
               escreval(numero, " eh primo");
            senao
               escreval(numero, " nao eh primo");
            fimse

         caso 2:
            escreval("");
            escreva("Digite a quantidade de numeros primos: ");
            leia(quantidade);

            escreval("A soma dos ", quantidade, " primeiros numeros primos eh: ", somaPrimos(quantidade));

         caso 3:
            escreval("");
            escreva("Digite um numero: ");
            leia(numero);

            escreval("O fatorial de ", numero, " eh: ", fatorial(numero));

         caso 4:
            escreval("");
            escreva("Digite um numero: ");
            leia(numero);

            exibirTabuada(numero);

         caso 5:
            escreval("");
            escreval("Encerrando o programa...");
      fimescolha

      escreval("");
   ate opcao = 5;
fimprocedimento

inicio
   main;
fimalgoritmo
```

Este código em Portugol é um programa que permite ao usuário selecionar diferentes opções para executar diversas funcionalidades. Aqui estão as funcionalidades disponíveis:

1. Verificar se um número é primo: O programa solicita um número do usuário e verifica se ele é um número primo.

2. Calcular a soma dos primeiros n números primos: O programa solicita a quantidade de números primos que o usuário deseja somar e retorna a soma desses números.

3. Calcular o fatorial de um número: O programa solicita um número do usuário e calcula o fatorial desse número.

4. Exibir a tabuada de um número: O programa solicita um número do usuário e exibe a tabuada desse número.

5. Sair: Encerra o programa.

O código utiliza funções e procedimentos para organizar as diferentes funcionalidades e facilitar a leitura e manutenção do código. Cada opção é tratada em um caso diferente dentro de uma estrutura de escolha.