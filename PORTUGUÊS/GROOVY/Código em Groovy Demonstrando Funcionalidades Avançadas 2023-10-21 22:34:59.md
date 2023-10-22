Certamente! Aqui está um exemplo de código em Groovy que é complexo e diferenciado, demonstrando algumas funcionalidades avançadas da linguagem:

```groovy
import groovy.transform.CompileStatic

@CompileStatic
class ComplexCode {
    static void main(String[] args) {
        def lista = [1, 2, 3, 4, 5]
        
        // Filtrar apenas os números pares e multiplicá-los por 2
        def resultado = lista.findAll { it % 2 == 0 }.collect { it * 2 }
        
        // Criar uma função lambda para calcular a média de uma lista de números
        def calcularMedia = { lista ->
            def soma = lista.sum()
            soma / lista.size()
        }
        
        // Calcular a média dos números pares multiplicados por 2
        def media = calcularMedia(resultado)
        
        // Imprimir o resultado
        println "A média dos números pares multiplicados por 2 é: $media"
        
        // Criar uma classe interna para representar uma pessoa
        class Pessoa {
            String nome
            int idade
            
            Pessoa(String nome, int idade) {
                this.nome = nome
                this.idade = idade
            }
            
            String saudacao() {
                "Olá, meu nome é $nome e eu tenho $idade anos."
            }
        }
        
        // Criar uma lista de pessoas
        def pessoas = [
            new Pessoa("João", 25),
            new Pessoa("Maria", 30),
            new Pessoa("Pedro", 40)
        ]
        
        // Filtrar as pessoas com idade maior que 30 e imprimir suas saudações
        pessoas.findAll { it.idade > 30 }.each { pessoa ->
            println pessoa.saudacao()
        }
    }
}
```

Este código demonstra algumas características avançadas do Groovy. Primeiro, uma lista de números é filtrada para conter apenas os números pares e, em seguida, todos os elementos da lista filtrada são multiplicados por 2. 

Em seguida, é definida uma função lambda para calcular a média dos números em uma lista. A função é então utilizada para calcular a média dos números pares multiplicados por 2.

Em seguida, é criada uma classe interna chamada "Pessoa" que representa uma pessoa com nome e idade. É definido um construtor e um método de saudação na classe.

Uma lista de pessoas é criada e filtrada para incluir apenas aquelas com idade maior que 30. Em seguida, a saudação de cada pessoa é impressa.

Espero que este código complexo em Groovy demonstre algumas funcionalidades avançadas da linguagem e seja útil para você!