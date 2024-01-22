```portuguol
programa Calculadora {

    função principal() {
        variável número1, número2, resultado, operação;
        escreva("Digite o primeiro número: ");
        leia(número1);
        escreva("Digite o segundo número: ");
        leia(número2);
        escreva("Digite a operação (+, -, *, /): ");
        leia(operação);

        se (operação == "+") {
            resultado = número1 + número2;
        } senão se (operação == "-") {
            resultado = número1 - número2;
        } senão se (operação == "*") {
            resultado = número1 * número2;
        } senão se (operação == "/") {
            se (número2 == 0) {
                escreva("Divisão por zero não é permitida.");
            } senão {
                resultado = número1 / número2;
            }
        } senão {
            escreva("Operação inválida.");
        }

        escreva("Resultado: ", resultado);
    }

}
```

Este código em PORTUGOL cria uma calculadora simples que pode realizar as quatro operações básicas (+, -, *, /) com dois números. O código usa a função `leia()` para ler os números e a operação do usuário, e a função `escreva()` para exibir o resultado.

O código também usa a instrução `se` para verificar se a operação é válida e para calcular o resultado. A instrução `se` também é usada para verificar se o segundo número é zero antes de realizar a divisão, para evitar uma divisão por zero.

O código também usa a instrução `função` para criar uma função chamada `principal()`, que é a função principal do programa. A função `principal()` contém o código principal do programa, que é executado quando o programa é iniciado.