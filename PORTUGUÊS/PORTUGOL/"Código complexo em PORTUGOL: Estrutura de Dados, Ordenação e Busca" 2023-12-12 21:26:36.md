Claro! Aqui está um código complexo em PORTUGOL que envolve a criação de uma estrutura de dados e a implementação de algoritmos de ordenação e busca:

```
algoritmo ProgramaComplexo;

var
   vetor: vetor[1..100] de inteiro;
   tamanhoVetor: inteiro;
   opcao: caractere;

funcao menuPrincipal(): caractere;
inicio
   escreva("---- MENU PRINCIPAL ----");
   escreva("1. Preencher vetor");
   escreva("2. Ordenar vetor");
   escreva("3. Pesquisar elemento no vetor");
   escreva("0. Sair");
   escreva("-------------------------");
   escreva("");

   escreva("Escolha uma opcao: ");
   leia(opcao);

   retorne opcao;
fim

procedimento preencherVetor(var vetor: vetor[1..100] de inteiro; var tamanhoVetor: inteiro);
var
   i: inteiro;
inicio
   escreva("Digite o tamanho do vetor: ");
   leia(tamanhoVetor);

   para i de 1 ate tamanhoVetor faca
   inicio
      escreva("Digite o elemento ", i, ": ");
      leia(vetor[i]);
   fimpara
fim

procedimento exibirVetor(vetor: vetor[1..100] de inteiro; tamanhoVetor: inteiro);
var
   i: inteiro;
inicio
   para i de 1 ate tamanhoVetor faca
   inicio
      escreva(vetor[i], " ");
   fimpara
   escreva("");
fim

procedimento ordenarVetor(var vetor: vetor[1..100] de inteiro; tamanhoVetor: inteiro);
var
   i, j, aux: inteiro;
inicio
   para i de 1 ate tamanhoVetor-1 faca
   inicio
      para j de 1 ate tamanhoVetor-i faca
      inicio
         se vetor[j] > vetor[j+1] entao
         inicio
            aux <- vetor[j];
            vetor[j] <- vetor[j+1];
            vetor[j+1] <- aux;
         fimse
      fimpara
   fimpara

   escreva("Vetor ordenado: ");
   exibirVetor(vetor, tamanhoVetor);
fim

funcao buscaBinaria(vetor: vetor[1..100] de inteiro; tamanhoVetor: inteiro; elemento: inteiro): inteiro;
var
   esquerda, direita, meio: inteiro;
inicio
   esquerda <- 1;
   direita <- tamanhoVetor;

   enquanto esquerda <= direita faca
   inicio
      meio <- (esquerda + direita) / 2;

      se vetor[meio] = elemento entao
         retorne meio;

      se vetor[meio] < elemento entao
         esquerda <- meio + 1;
      senao
         direita <- meio - 1;
   fimenquanto

   retorne -1; // elemento nao encontrado
fim

// Programa principal
inicio
   opcao <- menuPrincipal();

   enquanto opcao <> "0" faca
   inicio
      se opcao = "1" entao
      inicio
         preencherVetor(vetor, tamanhoVetor);
         exibirVetor(vetor, tamanhoVetor);
      fimse

      se opcao = "2" entao
      inicio
         ordenarVetor(vetor, tamanhoVetor);
      fimse

      se opcao = "3" entao
      inicio
         inteiro elemento, posicao;
         escreva("Digite o elemento a ser pesquisado: ");
         leia(elemento);

         posicao <- buscaBinaria(vetor, tamanhoVetor, elemento);

         se posicao <> -1 entao
         inicio
            escreva("Elemento encontrado na posicao ", posicao);
         fimse
         senao
         inicio
            escreva("Elemento nao encontrado");
         fimse
      fimse

      escreva("");
      opcao <- menuPrincipal();
   fimenquanto

   escreva("Encerrando o programa...");
fim.
```

Este código apresenta um programa com um menu principal que permite ao usuário realizar três ações: preencher um vetor, ordenar o vetor e pesquisar um elemento no vetor. 

A função `menuPrincipal()` exibe o menu e retorna a opção escolhida pelo usuário. O procedimento `preencherVetor()` solicita ao usuário o tamanho do vetor e preenche-o com elementos. O procedimento `exibirVetor()` exibe todos os elementos do vetor. O procedimento `ordenarVetor()` utiliza o algoritmo Bubble Sort para ordenar o vetor em ordem crescente. A função `buscaBinaria()` realiza a busca binária para encontrar um elemento específico no vetor e retorna a posição do elemento, ou -1 se não for encontrado.

No programa principal, é feita a leitura da opção escolhida pelo usuário e, de acordo com a opção selecionada, as ações correspondentes são executadas. O programa continua em um loop até que a opção "0" seja escolhida, encerrando o programa.

Espero que esse código atenda às suas expectativas!