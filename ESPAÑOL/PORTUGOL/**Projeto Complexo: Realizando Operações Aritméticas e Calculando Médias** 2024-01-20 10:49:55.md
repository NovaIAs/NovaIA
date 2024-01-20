```portuguol
programa projeto_complexo

definir a = 10
definir b = 20
definir c = 30

função somar(numero1, numero2)

    retornar numero1 + numero2

fim função

função multiplicar(numero1, numero2)

    retornar numero1 * numero2

fim função

função calcular_media(numero1, numero2, numero3)

    retornar (numero1 + numero2 + numero3) / 3

fim função

função imprimir_resultado(resultado)

    escreva resultado

fim função

início

    definir resultado_soma = somar(a, b)
    imprimir_resultado(resultado_soma)

    definir resultado_multiplicacao = multiplicar(b, c)
    imprimir_resultado(resultado_multiplicacao)

    definir resultado_media = calcular_media(a, b, c)
    imprimir_resultado(resultado_media)

fim programa
```

Explicação do código:

* O programa define três variáveis: `a`, `b` e `c`, com os valores 10, 20 e 30, respectivamente.
* O programa define três funções: `somar`, `multiplicar` e `calcular_media`.
* A função `somar` recebe dois números e retorna a soma dos dois números.
* A função `multiplicar` recebe dois números e retorna o produto dos dois números.
* A função `calcular_media` recebe três números e retorna a média dos três números.
* O programa define uma função `imprimir_resultado` que recebe um número e imprime o número na tela.
* O programa chama a função `somar` com os argumentos `a` e `b` e imprime o resultado na tela.
* O programa chama a função `multiplicar` com os argumentos `b` e `c` e imprime o resultado na tela.
* O programa chama a função `calcular_media` com os argumentos `a`, `b` e `c` e imprime o resultado na tela.