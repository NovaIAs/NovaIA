```portuguol
início

   inteiro A, B, C, aux, aux2, conta, contb;
   lógico achou, achou2;
   cadeia palavra;

   escreva("Digite uma palavra: ");
   leia(palavra);

   // Cálculo da quantidade de vogais e consoantes da palavra

   achou = falso;

   para A := 1 até comprimento(palavra) passo 1 faça
      achou := falso;

      para B := 65 até 90 passo 1 faça
         se letra(palavra[A]) = B ou letra(palavra[A]) = B + 32 então
            conta := conta + 1;
            achou := verdadeiro;
         fim-se
      fim-para

      se achou = falso então
         contb := contb + 1;
      fim-se
   fim-para

   escreva("\nQuantidade de vogais: ", conta);
   escreva("\nQuantidade de consoantes: ", contb);

   // Inversão da palavra

   aux := comprimento(palavra);

   para A := 1 até (aux / 2) passo 1 faça
      aux2 := palavra[A];
      palavra[A] := palavra[aux - A + 1];
      palavra[aux - A + 1] := aux2;
   fim-para

   // Verificação se a palavra é palíndroma

   achou2 := verdadeiro;

   para A := 1 até comprimento(palavra) passo 1 faça
      se palavra[A] <> palavra[aux - A + 1] então
         achou2 := falso;
      fim-se
   fim-para

   se achou2 = verdadeiro então
      escreva("\nA palavra é palíndroma.");
   senão
      escreva("\nA palavra não é palíndroma.");
   fim-se

fim
```

Este código faz o seguinte:

* Recebe uma palavra do utilizador;
* Calcula a quantidade de vogais e consoantes da palavra;
* Inverte a palavra;
* Verifica se a palavra é palíndroma (se lê da mesma forma da esquerda para a direita e da direita para a esquerda).

O código usa as seguintes variáveis:

* `A`, `B`, `C`, `aux`, `aux2`, `conta`, `contb`: variáveis inteiras;
* `achou`, `achou2`: variáveis lógicas;
* `palavra`: variável cadeia de caracteres.

O código usa os seguintes procedimentos:

* `escreva`: procedimento que escreve uma mensagem na tela;
* `leia`: procedimento que lê uma entrada do utilizador;
* `comprimento`: função que retorna o comprimento de uma cadeia de caracteres;
* `letra`: função que retorna a letra maiúscula ou minúscula correspondente a um código ASCII;
* `para`: estrutura de repetição;
* `se`: estrutura de seleção;
* `senão`: estrutura de seleção;
* `fim`: estrutura de fim.

O código é complexo porque usa várias estruturas de controle e funções para realizar várias tarefas. O código também é longo porque tem que verificar cada caractere da palavra para calcular a quantidade de vogais e consoantes e para verificar se a palavra é palíndroma.