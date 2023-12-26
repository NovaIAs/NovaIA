Claro! Aqui está um código complexo em Portugol:

```
// Programa para calcular a média ponderada de alunos em uma disciplina

algoritmo "Média Ponderada"
var
   nomes: vetor[1..5] de caractere
   notas1: vetor[1..5] de real
   notas2: vetor[1..5] de real
   notas3: vetor[1..5] de real
   pesos1: vetor[1..5] de inteiro
   pesos2: vetor[1..5] de inteiro
   pesos3: vetor[1..5] de inteiro
   medias: vetor[1..5] de real
   mediaTurma: real
   somaPesos: inteiro
   i: inteiro

funcao CalcularMediaPonderada(notas1: real, notas2: real, notas3: real, pesos1: inteiro, pesos2: inteiro, pesos3: inteiro): real
var
   somaPesos: inteiro
begin
   somaPesos <- pesos1 + pesos2 + pesos3
   retorne ((notas1 * pesos1) + (notas2 * pesos2) + (notas3 * pesos3)) / somaPesos
fimFuncao

inicio
   escreva("Digite os nomes dos alunos:")
   para i de 1 ate 5 faca
      leia(nomes[i])
   fimPara
   
   escreva("Digite as notas e pesos das 3 avaliacoes para cada aluno:")
   para i de 1 ate 5 faca
      escreva("Aluno ", i, ":")
      leia(notas1[i])
      leia(pesos1[i])
      leia(notas2[i])
      leia(pesos2[i])
      leia(notas3[i])
      leia(pesos3[i])
   fimPara
   
   mediaTurma <- 0
   somaPesos <- 0
   
   para i de 1 ate 5 faca
      medias[i] <- CalcularMediaPonderada(notas1[i], notas2[i], notas3[i], pesos1[i], pesos2[i], pesos3[i])
      mediaTurma <- mediaTurma + medias[i]
      somaPesos <- somaPesos + (pesos1[i] + pesos2[i] + pesos3[i])
   fimPara
   
   mediaTurma <- mediaTurma / 5
   
   escreva("Média Ponderada da Turma: ", mediaTurma)
   
   para i de 1 ate 5 faca
      escreva("Aluno ", i, ": ", medias[i])
   fimPara
fim
```

Neste código, desenvolvi um programa em Portugol para calcular a média ponderada de alunos em uma disciplina. O programa solicita ao usuário que digite os nomes dos alunos e, em seguida, pede as notas e os pesos das três avaliações para cada aluno.

Em seguida, é definida uma função chamada "CalcularMediaPonderada" que recebe as notas e os pesos das três avaliações como parâmetros e retorna a média ponderada calculada.

No bloco principal do programa, é utilizado um loop para calcular a média ponderada de cada aluno, somando suas médias individuais para calcular a média da turma. Além disso, é feita a soma dos pesos de todas as avaliações.

Por fim, são exibidas a média ponderada da turma e as médias individuais de cada aluno.

Espero que esse código atenda às suas expectativas!