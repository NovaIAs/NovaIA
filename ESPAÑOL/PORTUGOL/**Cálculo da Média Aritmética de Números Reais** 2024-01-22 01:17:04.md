```portuguol
INÍCIO
  ESCREVA("******************************************************************")
  ESCREVA("*                                                                *")
  ESCREVA("*    Programa: Cálculo da Média Aritmética de Números Reais    *")
  ESCREVA("*                                                                *")
  ESCREVA("******************************************************************")
  ESCREVALINHABRANCA

  VARIÁVEL qtdNumerosINTEIRO
  VARIÁVEL somaReaisREAL
  VARIÁVEL médiaReaisREAL

  ESCREVA("Digite a quantidade de números reais que você deseja calcular a média: ")
  LEIACONVERSOR(qtdNumerosINTEIRO)

  PARA iINTEIRO ← 1 ATÉ qtdNumerosINTEIRO FAÇA
    ESCREVA("Digite o ", iINTEIRO, "º número real: ")
    LEIACONVERSOR(somaReaisREAL)
  FIM-PARA

  médiaReaisREAL ← somaReaisREAL / qtdNumerosINTEIRO

  ESCREVALINHABRANCA
  ESCREVA("A média aritmética dos números reais digitados é: ", médiaReaisREAL)

  ESPERA TECLA
FIM
```

**Explicação do código:**

O código acima é um programa em PORTUGOL que calcula a média aritmética de um conjunto de números reais digitados pelo usuário.

O programa começa escrevendo uma mensagem de boas-vindas e pedindo ao usuário que digite a quantidade de números reais que deseja calcular a média.

Em seguida, o programa entra em um loop que se repete tantas vezes quanto o número de números reais que o usuário deseja calcular a média.

Dentro do loop, o programa pede ao usuário que digite cada número real e o armazena na variável `somaReaisREAL`.

Após o loop, o programa calcula a média aritmética dos números reais digitados dividindo a soma dos números pelo número de números.

Por fim, o programa escreve a média aritmética dos números reais digitados pelo usuário.