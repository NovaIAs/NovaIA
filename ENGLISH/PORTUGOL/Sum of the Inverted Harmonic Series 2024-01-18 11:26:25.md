```portugol
programa ExemploComplexo {

    funcao potencia(base: real, expoente: inteiro): real {
        se expoente = 0 entao
            retorne 1
        senao
            retorne base * potencia(base, expoente-1)
        fim-se
    }

    constante numero_de_termos = 100

    real soma = 0

    para i de 1 ate numero_de_termos passo 1 faca {
        real termo = 1 / potencia(2, i)
        soma += termo
    }

    escreva("A soma dos ", numero_de_termos, " primeiros termos da serie harmonica invertida eh ", soma)
}
```

Explicação:

* A função `potencia` calcula a potência de um número `base` elevado a um expoente `expoente`.
* A constante `numero_de_termos` define o número de termos da série harmônica invertida a serem somados.
* A variável `soma` é usada para acumular a soma dos termos da série.
* O laço `para` percorre os termos da série harmônica invertida de 1 a `numero_de_termos`, calculando e adicionando cada termo à variável `soma`.
* A função `escreva()` imprime o resultado da soma dos termos da série harmônica invertida na tela.