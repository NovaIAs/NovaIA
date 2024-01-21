```portuguol
Programa Calcula_area

Associar area com 0.0
Associar base com 0.0
Associar altura com 0.0
Associar forma com "quadrado"

Repetir enquanto forma não for igual a "sair":

    Escrever "Qual a forma geométrica que deseja calcular a área?"
    Ler forma

    Se forma for igual a "quadrado":
        Escrever "Informe o valor da base do quadrado em cm:"
        Ler base
        Escrever "Informe o valor da altura do quadrado em cm:"
        Ler altura
        Associar area com base * altura

    Se forma for igual a "retângulo":
        Escrever "Informe o valor da base do retângulo em cm:"
        Ler base
        Escrever "Informe o valor da altura do retângulo em cm:"
        Ler altura
        Associar area com base * altura

    Se forma for igual a "triângulo":
        Escrever "Informe o valor da base do triângulo em cm:"
        Ler base
        Escrever "Informe o valor da altura do triângulo em cm:"
        Ler altura
        Associar area com (base * altura) / 2

    Se forma for igual a "círculo":
        Escrever "Informe o valor do raio do círculo em cm:"
        Ler raio
        Associar area com 3.14159 * (raio * raio)

    Escrever "A área da forma geométrica é:", area, "cm²"

Fim programa
```

Este programa calcula a área de uma forma geométrica (quadrado, retângulo, triângulo ou círculo) a partir dos seus valores de base e altura (ou raio, no caso do círculo).

O programa começa por declarar as variáveis que serão utilizadas:

* **area**: a área da forma geométrica
* **base**: a base da forma geométrica
* **altura**: a altura da forma geométrica
* **forma**: a forma geométrica que estamos calculando a área

Em seguida, o programa entra num loop que se repete enquanto o utilizador não introduzir a palavra "sair". Dentro deste loop, o programa pergunta ao utilizador qual a forma geométrica que deseja calcular a área e lê a sua resposta.

Depois, o programa verifica qual a forma geométrica que o utilizador introduziu e, dependendo da forma, pede ao utilizador para introduzir os valores da base e da altura (ou do raio, no caso do círculo).

Com estes valores, o programa calcula a área da forma geométrica e escreve o resultado no ecrã.

Finalmente, o programa pergunta ao utilizador se deseja calcular a área de outra forma geométrica e, se o utilizador responder "sim", volta ao início do loop. Se o utilizador responder "não", o programa termina.