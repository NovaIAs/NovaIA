Claro! Aqui está um exemplo de um código em Groovy que utiliza conceitos avançados de programação e é bastante complexo:

```groovy
// Classe principal
class Programa {

    static void main(String[] args) {
        def listaNumeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        
        // Filtra os números pares
        def numerosPares = listaNumeros.findAll { it % 2 == 0 }
        
        // Aplica a função fatorial nos números pares
        def numerosFatoriais = numerosPares.collect { fatorial(it) }
        
        // Imprime os números fatoriais
        numerosFatoriais.each { num ->
            println("O fatorial de $num é ${fatorial(num)}")
        }
    }
    
    // Função recursiva para cálculo do fatorial
    static int fatorial(int num) {
        if (num <= 1) {
            return 1
        } else {
            return num * fatorial(num - 1)
        }
    }
}
```

Neste código, começamos definindo uma classe chamada `Programa` que possui um método `main`, que é o ponto de entrada do programa. 

Dentro do método `main`, criamos uma lista de números chamada `listaNumeros` contendo os números de 1 a 10. 

Em seguida, utilizamos o método `findAll` para filtrar apenas os números pares da lista. 

Depois, utilizamos o método `collect` para aplicar a função `fatorial` em cada número par da lista, armazenando o resultado na lista `numerosFatoriais`. 

Por fim, utilizamos o método `each` para percorrer a lista `numerosFatoriais` e imprimir na tela o número e seu respectivo fatorial. 

A função `fatorial` é uma função recursiva que realiza o cálculo do fatorial de um número. Ela é implementada utilizando uma estrutura condicional `if` para tratar o caso base (quando o número é menor ou igual a 1) e uma chamada recursiva para calcular o fatorial do número decrementado em 1.

Espero que esse código atenda às suas expectativas!