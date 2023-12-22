Claro! Aqui está um código complexo em Groovy que envolve várias funcionalidades e explicações detalhadas:

```groovy
// Importando bibliotecas necessárias
import java.util.Random

// Definindo uma classe chamada Complexo
class Complexo {
    int real
    int imaginario
    
    // Construtor da classe Complexo
    Complexo(int real, int imaginario) {
        this.real = real
        this.imaginario = imaginario
    }
    
    // Método para somar dois números complexos
    Complexo somar(Complexo c) {
        int somaReal = this.real + c.real
        int somaImaginario = this.imaginario + c.imaginario
        return new Complexo(somaReal, somaImaginario)
    }
    
    // Método para subtrair dois números complexos
    Complexo subtrair(Complexo c) {
        int subtracaoReal = this.real - c.real
        int subtracaoImaginario = this.imaginario - c.imaginario
        return new Complexo(subtracaoReal, subtracaoImaginario)
    }
    
    // Método para multiplicar dois números complexos
    Complexo multiplicar(Complexo c) {
        int multiplicacaoReal = (this.real * c.real) - (this.imaginario * c.imaginario)
        int multiplicacaoImaginario = (this.real * c.imaginario) + (this.imaginario * c.real)
        return new Complexo(multiplicacaoReal, multiplicacaoImaginario)
    }
    
    // Método para gerar um número complexo aleatório
    static Complexo gerarComplexoAleatorio() {
        Random random = new Random()
        int realAleatorio = random.nextInt(100)
        int imaginarioAleatorio = random.nextInt(100)
        return new Complexo(realAleatorio, imaginarioAleatorio)
    }
}

// Criando dois números complexos
Complexo numero1 = Complexo.gerarComplexoAleatorio()
Complexo numero2 = Complexo.gerarComplexoAleatorio()

// Imprimindo os números complexos gerados
println "Número complexo 1: ${numero1.real} + ${numero1.imaginario}i"
println "Número complexo 2: ${numero2.real} + ${numero2.imaginario}i"

// Somando os números complexos
Complexo soma = numero1.somar(numero2)
println "Soma dos números complexos: ${soma.real} + ${soma.imaginario}i"

// Subtraindo os números complexos
Complexo subtracao = numero1.subtrair(numero2)
println "Subtração dos números complexos: ${subtracao.real} + ${subtracao.imaginario}i"

// Multiplicando os números complexos
Complexo multiplicacao = numero1.multiplicar(numero2)
println "Multiplicação dos números complexos: ${multiplicacao.real} + ${multiplicacao.imaginario}i"
```

Neste código, criamos uma classe chamada `Complexo` que representa números complexos. A classe tem um construtor que recebe as partes real e imaginária do número complexo, e também possui métodos para realizar operações como soma, subtração e multiplicação entre números complexos.

Além disso, também temos um método estático `gerarComplexoAleatorio`, que utiliza a classe `Random` para gerar números complexos aleatórios.

No programa principal, criamos dois números complexos aleatórios e realizamos as operações de soma, subtração e multiplicação entre eles. O resultado é então impresso na tela.