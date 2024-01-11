```portuguol
programa calcula_area_triangulo

funcao area_triangulo(base, altura)
    area <- (base * altura) / 2
    retorna area
fim_funcao

funcao main()
    base <- ler_valor("Informe a base do triangulo: ")
    altura <- ler_valor("Informe a altura do triangulo: ")
    area <- area_triangulo(base, altura)
    escrever_valor("A area do triangulo e ", area)
fim_funcao
```

**Explicação do código:**

* **Função `area_triangulo`:** Esta função calcula a área de um triângulo a partir de sua base e altura. Ela recebe dois parâmetros, `base` e `altura`, e retorna a área calculada.
* **Função `main`:** Esta é a função principal do programa. Ela chama a função `area_triangulo` para calcular a área de um triângulo e, em seguida, exibe o resultado na tela.

**Uso do código:**

Para usar este código, você pode copiá-lo para um editor de texto e salvá-lo com a extensão `.por`. Em seguida, você pode abrir o arquivo no interpretador do PORTUGOL e executá-lo.

**Exemplo:**

Se você executar o código acima, ele solicitará que você informe a base e a altura do triângulo. Após informar esses valores, o programa calculará e exibirá a área do triângulo.

```
Informe a base do triangulo: 10
Informe a altura do triangulo: 5
A area do triangulo e 25
```