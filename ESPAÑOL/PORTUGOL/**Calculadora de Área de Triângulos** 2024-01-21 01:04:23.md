```portuguol
programa Triangulador
    funcao calculo_area(base, altura)
        area := (base * altura) / 2
        retorna area
    fim_funcao

    funcao exibe_area(area)
        escreve("A área do triângulo é ", area)
    fim_funcao

    inicio
        base := solicita_real("Informe a base do triângulo: ")
        altura := solicita_real("Informe a altura do triângulo: ")
        area := calculo_area(base, altura)
        exibe_area(area)
    fim

funcao solicita_real(mensagem)
    real := solicita(mensagem)
    verifica_real(real)
    retorna real
fim_funcao

funcao solicita(mensagem)
    escreva(mensagem)
    leia(texto)
    retorna texto
fim_funcao

funcao verifica_real(texto)
    se nao eh_real(texto) entao
        escreva("Erro: o valor informado não é um número real.")
        termina
    fim_se
fim_funcao

funcao eh_real(texto)
    booleano := 1
    para cada caractere em texto faca
        se caractere nao eh numero e caractere nao eh ponto entao
            booleano := 0
            sai
        fim_se
    fim_para
    retorna booleano
fim_funcao
```

Este código solicita ao utilizador a base e a altura de um triângulo e utiliza uma função para calcular a área do triângulo e mostrá-la ao utilizador.

O código utiliza as seguintes funções:

* `calculo_area(base, altura)`: calcula a área de um triângulo com base e altura dadas.
* `exibe_area(area)`: exibe a área do triângulo na consola.
* `solicita_real(mensagem)`: solicita ao utilizador um número real e verifica se é um número real válido.
* `solicita(mensagem)`: solicita ao utilizador uma string e retorna a string.
* `verifica_real(texto)`: verifica se a string é um número real válido.
* `eh_real(texto)`: verifica se a string é um número real válido.

O código principal do programa é o seguinte:

```portuguol
base := solicita_real("Informe a base do triângulo: ")
altura := solicita_real("Informe a altura do triângulo: ")
area := calculo_area(base, altura)
exibe_area(area)
```

Este código solicita ao utilizador a base e a altura do triângulo, chama a função `calculo_area` para calcular a área do triângulo e chama a função `exibe_area` para mostrar a área do triângulo na consola.