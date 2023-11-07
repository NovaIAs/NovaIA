Claro! Aqui está um código complexo em Portugol que realiza o cálculo do fatorial de um número usando recursão:

```
algoritmo FatorialRecursivo

funcao fatorial(numero: inteiro): inteiro
inicio
    se numero = 0 entao
        retorne 1
    senao
        retorne numero * fatorial(numero - 1)
    fimse
fimfuncao

inicio
    escreva("Digite um número inteiro: ")
    leia(numero)

    resultado := fatorial(numero)

    escreva("O fatorial de ", numero, " é ", resultado)
fimalgoritmo
```

Neste código, temos uma função chamada "fatorial" que recebe como parâmetro um número inteiro e retorna seu fatorial. A função é implementada de forma recursiva, ou seja, ela chama a si mesma dentro do seu próprio corpo.

O algoritmo principal começa solicitando ao usuário que digite um número inteiro. Em seguida, o número é lido e armazenado na variável "numero". A função "fatorial" é chamada, passando o valor de "numero" como argumento, e o resultado é armazenado na variável "resultado". Por fim, o algoritmo exibe o resultado na tela.

A recursão utilizada nesse código é uma técnica poderosa em programação, mas também pode ser complexa para entender em um primeiro momento. Ela permite que a função se chame repetidamente até atingir uma condição de parada, nesse caso, quando o número passado for igual a zero. A cada chamada recursiva, o número é decrementado em uma unidade, garantindo que a função será chamada para valores menores até chegar ao caso base, onde o fatorial de 0 é 1.