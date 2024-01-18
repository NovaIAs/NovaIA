```portugol
program CalculaSoma {

    // Declara as variáveis utilizadas no programa
    inteiro i, somatorio, num;
    real media;

    // Solicita ao utilizador o número de elementos da soma
    escreva("Informe o número de elementos da soma: ");
    leia(num);

    // Inicializa o somatório e o contador
    somatorio := 0;
    i := 1;

    // Realiza a soma dos elementos
     enquanto (i <= num) faça {
        escreva("Informe o ", i, "º elemento: ");
        leia(elemento);
        somatorio := somatorio + elemento;
        i := i + 1;
    }

    // Calcula a média dos elementos
    media := somatorio / num;

    // Exibe o resultado
    escreva("A soma dos elementos é: ", somatorio);
    escreva("A média dos elementos é: ", media);

}
```

**Explicação do código:**

* O programa CalculaSoma é um programa que solicita ao utilizador o número de elementos da soma e, em seguida, solicita ao utilizador o valor de cada um dos elementos. O programa calcula a soma dos elementos e a média dos elementos.
* O programa começa por declarar as variáveis utilizadas no programa. As variáveis declaradas são:
    * `i`: uma variável inteira que é utilizada para controlar o loop que realiza a soma dos elementos.
    * `somatorio`: uma variável inteira que é utilizada para armazenar o valor da soma dos elementos.
    * `num`: uma variável inteira que é utilizada para armazenar o número de elementos da soma.
    * `media`: uma variável real que é utilizada para armazenar o valor da média dos elementos.
* O programa solicita ao utilizador o número de elementos da soma utilizando a função `escreva`. A função `escreva` imprime uma mensagem na consola. O programa utiliza a função `leia` para ler a entrada do utilizador. A função `leia` lê uma linha de texto da consola e converte-a para o tipo de dados especificado.
* O programa inicializa o somatório e o contador utilizando as instruções `somatorio := 0` e `i := 1`. A instrução `somatorio := 0` define o valor da variável `somatorio` para 0. A instrução `i := 1` define o valor da variável `i` para 1.
* O programa realiza a soma dos elementos utilizando um loop `enquanto`. O loop `enquanto` é executado enquanto a condição `i <= num` for verdadeira. A condição `i <= num` é verdadeira se o valor da variável `i` for menor ou igual ao valor da variável `num`.
* Dentro do loop `enquanto`, o programa solicita ao utilizador o valor do elemento atual utilizando a função `escreva`. A função `escreva` imprime uma mensagem na consola. O programa utiliza a função `leia` para ler a entrada do utilizador. A função `leia` lê uma linha de texto da consola e converte-a para o tipo de dados especificado.
* O programa adiciona o valor do elemento atual ao somatório utilizando a instrução `somatorio := somatorio + elemento`. A instrução `somatorio := somatorio + elemento` define o valor da variável `somatorio` para o valor da variável `somatorio` mais o valor da variável `elemento`.
* O programa incrementa o valor da variável `i` utilizando a instrução `i := i + 1`. A instrução `i := i + 1` define o valor da variável `i` para o valor da variável `i` mais 1.
* O programa calcula a média dos elementos utilizando a instrução `media := somatorio / num`. A instrução `media := somatorio / num` define o valor da variável `media` para o valor da variável `somatorio` dividido pelo valor da variável `num`.
* O programa exibe o resultado utilizando as funções `escreva`. As funções `escreva` imprimem uma mensagem na consola. O programa utiliza a função `escreva` para imprimir o valor do somatório e da média dos elementos.