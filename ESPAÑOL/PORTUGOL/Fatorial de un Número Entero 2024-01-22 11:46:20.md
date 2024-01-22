```portuguol
// Programa em PORTUGOL para calcular e imprimir o fatorial de um número inteiro

// Declaração das variáveis
inteiro n, i, fat;

// Imprime uma mensagem na tela, solicitando ao usuário que digite um número inteiro
imprima("Digite um número inteiro:");

// Lê o número inteiro digitado pelo usuário
leia(n);

// Inicializa a variável fat com o valor 1
fat := 1;

// Calcula o fatorial de n
para i de 1 até n faça
    fat := fat * i;
fim_para

// Imprime o fatorial de n na tela
imprima("O fatorial de ", n, " é igual a ", fat);
```

Explicação do código:

* A primeira linha do código é um comentário, que explica o propósito do programa.
* A segunda linha declara as variáveis que serão utilizadas no programa.
* A terceira linha imprime uma mensagem na tela, solicitando ao usuário que digite um número inteiro.
* A quarta linha lê o número inteiro digitado pelo usuário e o armazena na variável n.
* A quinta linha inicializa a variável fat com o valor 1.
* A sexta linha calcula o fatorial de n utilizando um laço de repetição.
* O laço de repetição começa em 1 e vai até n.
* Em cada iteração do laço, o valor de fat é multiplicado pelo valor de i.
* A sétima linha imprime o fatorial de n na tela.