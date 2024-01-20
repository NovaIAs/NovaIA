```portugol
# Programa que calcula a soma dos números pares de 1 a 100.
soma = 0
para i de 1 até 100 faça
    se i módulo 2 for igual a 0 então
        soma = soma + i
    fim_se
fim_para
escreva("A soma dos números pares de 1 a 100 é ", soma)

# Programa que calcula o fatorial de um número.
fatorial = 1
escreva("Digite um número: ")
leia(numero)
para i de 1 até numero faça
    fatorial = fatorial * i
fim_para
escreva("O fatorial de ", numero, " é ", fatorial)

# Programa que imprime uma tabela de multiplicação.
para i de 1 até 10 faça
    para j de 1 até 10 faça
        escreva(i * j, "\t")
    fim_para
    escreva("\n")
fim_para

# Programa que imprime um triângulo de asteriscos.
altura = 5
para i de 1 até altura faça
    para j de 1 até i faça
        escreva("*")
    fim_para
    escreva("\n")
fim_para

# Programa que imprime um jogo da velha.
tabuleiro = [
    [".", ".", "."],
    [".", ".", "."],
    [".", ".", "."]
]

# Imprime o tabuleiro.
para i de 1 até 3 faça
    para j de 1 até 3 faça
        escreva(tabuleiro[i - 1][j - 1], "\t")
    fim_para
    escreva("\n")
fim_para

# Lê a jogada do jogador.
escreva("Digite a sua jogada (linha, coluna): ")
leia(linha, coluna)

# Verifica se a jogada é válida.
se linha < 1 ou linha > 3 ou coluna < 1 ou coluna > 3 então
    escreva("Jogada inválida.")
fim_se

# Verifica se a célula está vazia.
se tabuleiro[linha - 1][coluna - 1] != "." então
    escreva("Célula ocupada.")
fim_se

# Faz a jogada.
tabuleiro[linha - 1][coluna - 1] = "X"

# Imprime o tabuleiro.
para i de 1 até 3 faça
    para j de 1 até 3 faça
        escreva(tabuleiro[i - 1][j - 1], "\t")
    fim_para
    escreva("\n")
fim_para
```

O código acima é um conjunto de programas em PORTUGOL que realizam diversas tarefas, como calcular a soma dos números pares de 1 a 100, calcular o fatorial de um número, imprimir uma tabela de multiplicação, imprimir um triângulo de asteriscos e imprimir um jogo da velha.

O primeiro programa usa um loop `para` para iterar sobre os números de 1 a 100 e adiciona os números pares à variável `soma`.

O segundo programa usa um loop `para` para iterar sobre os números de 1 ao número inserido pelo usuário e multiplica cada número pelo anterior, armazenando o resultado na variável `fatorial`.

O terceiro programa usa dois loops `para` para iterar sobre os números de 1 a 10 e imprimir uma tabela de multiplicação.

O quarto programa usa um loop `para` para iterar sobre os números de 1 à altura inserida pelo usuário e usa um segundo loop `para` para imprimir os asteriscos em cada linha.

O quinto programa cria um tabuleiro de jogo da velha e usa um loop `para` para imprimir o tabuleiro. Em seguida, lê a jogada do jogador e verifica se ela é válida. Se a jogada for válida, ela é feita no tabuleiro e o tabuleiro é impresso novamente.