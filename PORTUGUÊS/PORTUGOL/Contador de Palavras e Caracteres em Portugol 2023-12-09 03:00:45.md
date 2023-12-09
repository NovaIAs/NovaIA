Claro! Vou criar um código em Portugol complexo que envolve manipulação de strings e estruturas de repetição. O código será um programa que lê uma frase digitada pelo usuário e retorna a quantidade de palavras e caracteres presentes nessa frase. Vamos lá:

```
algoritmo ContadorDePalavras
var
    frase: caractere
    qtdPalavras, qtdCaracteres: inteiro
    i: inteiro

inicio
    qtdPalavras <- 0
    qtdCaracteres <- 0

    escreva("Digite uma frase: ")
    leia(frase)

    para i de 1 ate tamanho(frase) faca
        se frase[i] = " " entao  // Verifica se o caractere atual é um espaço em branco
            qtdPalavras <- qtdPalavras + 1
        fimse

        // Verifica se o caractere atual não é um espaço em branco ou um caractere especial
        se nao (frase[i] = " " ou frase[i] = "." ou frase[i] = "," ou frase[i] = "!" ou frase[i] = "?" ou frase[i] = ";" ou frase[i] = ":") entao
            qtdCaracteres <- qtdCaracteres + 1
        fimse
    fimpara

    // Incrementa o contador de palavras para considerar a última palavra da frase
    qtdPalavras <- qtdPalavras + 1

    escreva("A frase possui ", qtdPalavras, " palavra(s) e ", qtdCaracteres, " caractere(s).")
fim
```

Explicação do código:
1. Primeiro, declaramos as variáveis `frase` para armazenar a frase digitada pelo usuário, `qtdPalavras` para contar a quantidade de palavras e `qtdCaracteres` para contar a quantidade de caracteres.
2. Inicializamos as variáveis `qtdPalavras` e `qtdCaracteres` com zero.
3. Pedimos ao usuário para digitar uma frase e lemos essa frase.
4. Utilizamos um laço `para` para percorrer cada caractere da frase.
5. Dentro do laço, verificamos se o caractere atual é um espaço em branco. Se for, incrementamos o contador de palavras `qtdPalavras`.
6. Em seguida, verificamos se o caractere atual não é um espaço em branco nem um caractere especial (como ponto, vírgula, exclamação, interrogação, ponto e vírgula, dois pontos). Se não for, incrementamos o contador de caracteres `qtdCaracteres`.
7. Após percorrer todos os caracteres da frase, incrementamos o contador de palavras `qtdPalavras` em 1 para considerar a última palavra da frase.
8. Por fim, exibimos a quantidade de palavras e caracteres presentes na frase.

Espero que esse código atenda às suas expectativas!