```portuguol
programa CalculadoraIMC {

    funcao real CalcularIMC(real peso, real altura) {
        real imc
        imc = peso / (altura * altura)
        retorna imc
    }

    funcao void ImprimirResultado(real imc) {
        se (imc < 18.5) {
            escreva("Abaixo do peso")
        } senão se (imc >= 18.5 e imc < 25) {
            escreva("Peso normal")
        } senão se (imc >= 25 e imc < 30) {
            escreva("Sobrepeso")
        } senão se (imc >= 30) {
            escreva("Obesidade")
        }
    }

    real peso, altura, imc
    escreva("Insira o seu peso (em kg): ")
    leia(peso)
    escreva("Insira a sua altura (em metros): ")
    leia(altura)
    imc = CalcularIMC(peso, altura)
    ImprimirResultado(imc)

}
```

**Explicação do código:**

1. **Função `CalcularIMC`:** Essa função calcula o índice de massa corporal (IMC) de uma pessoa a partir do seu peso e altura. Ela recebe dois parâmetros: `peso` e `altura`, e retorna o IMC calculado.
2. **Função `ImprimirResultado`:** Essa função imprime o resultado do cálculo do IMC. Ela recebe um parâmetro: `imc`, e imprime uma mensagem de acordo com o valor do IMC.
3. **Variáveis:** O programa usa as seguintes variáveis:
    * `peso`: armazena o peso da pessoa em kg.
    * `altura`: armazena a altura da pessoa em metros.
    * `imc`: armazena o IMC calculado.
4. **Entrada e saída de dados:** O programa usa as funções `escreva()` e `leia()` para ler os valores do peso e da altura do usuário.
5. **Chamada de funções:** O programa chama as funções `CalcularIMC()` e `ImprimirResultado()` para calcular e imprimir o IMC.
6. **Condições:** O programa usa instruções `se` para verificar o valor do IMC e imprimir a mensagem apropriada.

Este código é um exemplo de um programa simples em português que pode ser usado para calcular e imprimir o IMC de uma pessoa.