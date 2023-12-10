Claro! Aqui está um código em PORTUGOL que realiza a ordenação de um vetor utilizando o algoritmo de ordenação Bubble Sort:

```
algoritmo "Bubble Sort"
var
   vetor: vetor[1..10] de inteiro
   i, j, aux: inteiro

inicio
   // Preenchendo o vetor com valores aleatórios
   para i de 1 ate 10 faca
      vetor[i] <- aleatorio(1, 100)
   fimpara

   // Exibindo o vetor original
   escreva("Vetor original: ")
   para i de 1 ate 10 faca
      escreva(vetor[i], " ")
   fimpara
   escreval("\n")

   // Ordenando o vetor utilizando Bubble Sort
   para i de 1 ate 9 faca
      para j de 1 ate 10 - i faca
         se vetor[j] > vetor[j+1] entao
            // Troca os elementos de posição
            aux <- vetor[j]
            vetor[j] <- vetor[j+1]
            vetor[j+1] <- aux
         fimse
      fimpara
   fimpara

   // Exibindo o vetor ordenado
   escreva("Vetor ordenado: ")
   para i de 1 ate 10 faca
      escreva(vetor[i], " ")
   fimpara
fimalgoritmo
```

Explicação do código:
1. Primeiramente, declaramos o vetor `vetor` com tamanho 10 e as variáveis `i`, `j` e `aux` que serão utilizadas no processo de ordenação.
2. Em seguida, utilizamos um laço `para` para preencher o vetor com valores aleatórios, utilizando a função `aleatorio(1, 100)`.
3. Após preencher o vetor, exibimos o vetor original na tela.
4. Agora, vamos realizar a ordenação do vetor utilizando o algoritmo Bubble Sort. Utilizamos dois laços `para`, sendo o primeiro de `1` até `9`, que representa o número máximo de iterações necessárias para ordenar o vetor. O segundo laço `para` vai de `1` até `10 - i`, onde `i` representa a iteração atual do primeiro laço. Isso ocorre porque, a cada iteração do primeiro laço, o maior elemento é posicionado corretamente no final do vetor, então não é necessário comparar novamente.
5. Dentro do laço `para`, comparamos se o elemento `vetor[j]` é maior que o elemento `vetor[j+1]`. Caso seja verdadeiro, realizamos a troca dos elementos de posição utilizando uma variável auxiliar `aux`.
6. Por fim, exibimos o vetor ordenado na tela.

Espero que este código atenda às suas necessidades! Se tiver alguma dúvida ou precisar de mais exemplos, estou à disposição.