Claro! Aqui está um código complexo em Portugol que envolve uma série de cálculos matemáticos e manipulação de strings:

```
algoritmo Complexo
var
    nome: vetor[1..50] de caractere
    idade, num1, num2, resultado: inteiro

funcao fatorial(n: inteiro): inteiro
var
    fat: inteiro
    
inicio
    fat <- 1
    para i de 1 ate n faca
        fat <- fat * i
    fimpara
    retorne fat
fimfuncao

inicio
    escreva("Digite o seu nome: ")
    leia(nome)
    
    escreva("Digite sua idade: ")
    leia(idade)
    
    escreva("Digite um número inteiro: ")
    leia(num1)
    
    escreva("Digite outro número inteiro: ")
    leia(num2)
    
    resultado <- fatorial(num1) + fatorial(num2)
    
    escreva("Olá, ", nome, "! Seu nome tem ", tamanho(nome), " caracteres.")
    escreva("Você tem ", idade, " anos.")
    escreva("A soma dos fatoriais de ", num1, " e ", num2, " é igual a ", resultado)
    
    se resultado mod 2 = 0 entao
        escreva("O resultado é par.")
    senao
        escreva("O resultado é ímpar.")
    fimse
    
    se resultado > 100 entao
        escreva("O resultado é maior que 100.")
    senao se resultado < 100 entao
        escreva("O resultado é menor que 100.")
    senao
        escreva("O resultado é igual a 100.")
    fimse
fimalgoritmo
```

Neste código, o usuário é solicitado a digitar seu nome, idade e dois números inteiros. Em seguida, o código calcula o fatorial de cada número digitado utilizando a função `fatorial` e armazena a soma dos fatoriais na variável `resultado`. 

Depois disso, o código exibe uma mensagem de boas-vindas com o nome do usuário e informa o número de caracteres do nome. Em seguida, mostra a idade digitada pelo usuário e a soma dos fatoriais. 

Por fim, o código verifica se o resultado é par ou ímpar e se é maior, menor ou igual a 100, exibindo a mensagem correspondente.